-module(aikcp_pcb).

-include("aikcp.hrl").
-export([new/1,
         handle/1]).


new(Conv)-> #aikcp_pcb{conv = Conv}.
handle(PCB) -> handle(aikcp_util:millisecond(),PCB).

handle(Current,#aikcp_pcb{updated = true,
                      ts_flush = TSFlush,interval = Interval} = PCB) ->
  Slap = Current - TSFlush,
  {Slap2, TSFlush2} =
    case (Slap >= 10000) or (Slap < -10000) of
      true -> {0, Current};
      false -> {Slap, TSFlush}
    end,
  case Slap2 >= 0 of
    false -> {[],PCB#aikcp_pcb{current = Current,ts_flush = TSFlush2}};
    true  ->
      TSFlush3 = TSFlush2 + Interval,
      TSFlush4 =
        if ?DIFF_32(Current, TSFlush3) >= 0 -> Current + Interval;
           true -> TSFlush3
        end,
      flush(PCB#aikcp_pcb{current = Current,ts_flush = TSFlush4})
  end;

handle(Now,PCB) -> handle(Now,PCB#aikcp_pcb{current = Now,ts_flush = Now,updated = true}).

flush(#aikcp_pcb{ackcount = AckCount,rmt_wnd = RmtWnd,
                snd_next = SndNext,snd_una = SndUna} = PCB)->
  Wnd =  wnd(PCB), %% 自己的窗口
  {Buffer,PCB2} =
    if AckCount > 0 -> flush_ack(Wnd,PCB);
       true -> {[],PCB}
    end,
  PCB3 =
    if RmtWnd == 0 -> update_probe(PCB2);
       true -> PCB2#aikcp_pcb{probe_wait = 0,ts_probe = 0}
    end,
  {Buffer2,PCB4} =  flush_probe(Wnd,Buffer,PCB3),
  PWnd = pwnd(PCB4), %% 对方的窗口
  PCB5 =
    if ?DIFF_32(SndNext, ?BIT_32(SndUna + PWnd)) < 0 -> 
        queue_to_buffer(Wnd,PWnd,PCB4);
       true ->  PCB4
    end,
  {Buffer3,PCB6} = flush_data(Wnd,PWnd,Buffer2,PCB5),
  {lists:reverse(Buffer3),PCB6}.


flush_data(Wnd,PWnd,Buffer,
             #aikcp_pcb{fastresend = FastResend, nodelay = NoDelay,
                        rx_rto = RxRto,snd_buf = SndBuf} = PCB)->
  ReSent =
    if FastResend > 0 -> FastResend;
       true -> 16#FFFFFFFF
    end,
  RtoMin =
    if NoDelay == 0 -> RxRto bsr 3;
       true ->  0
    end,
  do_send(Wnd,PWnd,ReSent,RtoMin,Buffer,
            aikcp_buffer:head(SndBuf),false,false,PCB).
do_send(_Wnd,PWnd,ReSent,_RtoMin,
          Buffer,-1,Change,Lost,PCB)->
  PCB2 =
    case Change of
      false -> PCB;
      true  ->
        #aikcp_pcb{snd_next = SndNext, snd_una = SndUna, mss = MSS} = PCB,
        Thresh2 =
          case ?BIT_32(SndNext - SndUna) div 2 of
            V when V < ?KCP_THRESH_MIN -> ?KCP_THRESH_MIN;
            V -> V
          end,
        CWnd = Thresh2 + ReSent,
        PCB#aikcp_pcb{cwnd = CWnd,incr = CWnd * MSS, ssthresh = Thresh2}
    end,
  PCB3 =
    if Lost == true ->
        PCB2#aikcp_pcb{ssthresh = ?MAX(?KCP_THRESH_MIN, PWnd div 2),
                       cwnd = 1, incr = PCB2#aikcp_pcb.mss};
       true -> PCB2
    end,
  PCB4 =
    if PCB3#aikcp_pcb.cwnd < 1 ->
        PCB3#aikcp_pcb{cwnd = 1,incr = PCB3#aikcp_pcb.mss};
       true -> PCB3
    end,
  {Buffer,PCB4};

do_send(Wnd,PWnd,ReSent,RtoMin,Buffer,
          Idx,Change,Lost,
         #aikcp_pcb{current = Now,snd_buf = SndBuf,nodelay = NoDelay,
                    rx_rto = Rto,mtu = MTU, fastlimit = FastLimit,
                    rcv_next = RcvNext,xmit = PCBXmit,dead_link = DeadLink} = PCB)->
  Next = aikcp_buffer:next(Idx, SndBuf),
  Seg = aikcp_buffer:data(Idx,SndBuf),
  #aikcp_seg{xmit = Xmit, resendts = ResentTS,
             fastack = FastAck, rto = SegRto} = Seg,
  {NeedSend,Lost2,Change2,Seg2,XmitAcc} =
    if Xmit == 0 ->
        {true,Lost,Change,
         Seg#aikcp_seg{xmit = Xmit + 1, rto = Rto,
                       resendts = Now + Rto + RtoMin},0};
       ?DIFF_32(Now,ResentTS) >= 0 ->
        SegRto2 =
          if NoDelay == 0 -> SegRto + ?MAX(Rto,SegRto);
             NoDelay < 2 -> SegRto + SegRto div 2;
             true -> SegRto + Rto div 2
          end,
        {true,true,Change,
         Seg#aikcp_seg{xmit = Xmit + 1,rto = SegRto2,
                       resendts = Now + SegRto2},1};
       %% 支持快速重传，并且快速重传小于5次
       (FastAck >= ReSent)
       and ((Xmit < FastLimit)  or (FastLimit =< 0))->
        {true, Lost, true,
         Seg#aikcp_seg{xmit = Xmit + 1, fastack = 0,
                       resendts = Now + SegRto},0};
       true -> {false, Lost, Change, Seg,0}
    end,

  if NeedSend == true ->
      #aikcp_seg{conv = Conv, frg = Frg,
                 sn = Sn, len = Len, data = Data,xmit = SegXmit} = Seg2,
      Bin = ?KCP_SEG(Conv, ?KCP_CMD_PUSH, Frg, Wnd, Now, Sn,RcvNext, Len, Data, <<>>),
      Buffer2 = build_buffer(Bin,Buffer,MTU),
      SndBuf2 = aikcp_buffer:replace(Idx,Seg2#aikcp_seg{ts = Now,una = RcvNext}, SndBuf),
      State =
        if SegXmit >= DeadLink -> -1;
           true -> 0
        end,
      do_send(Wnd,PWnd,ReSent,RtoMin,Buffer2,Next,Change2,Lost2,
              PCB#aikcp_pcb{state = State,snd_buf = SndBuf2,xmit = PCBXmit + XmitAcc});
     true -> do_send(Wnd,PWnd,ReSent,RtoMin,Buffer,Next,Change2,Lost2,PCB)
  end.

queue_to_buffer(Wnd,PWnd,
           #aikcp_pcb{snd_queue = SndQ, snd_buf = SndBuf,
                      snd_next = SndNext, snd_una = SndUna} = PCB)->
  case aikcp_queue:size(SndQ) == 0 of
    true -> PCB;
    false -> queue_to_buffer(SndNext,?BIT_32(SndUna + PWnd),
                             SndQ,SndBuf,Wnd,PCB)
  end.

queue_to_buffer(SndNext,Limit,SndQ,SndBuf,Wnd,
                #aikcp_pcb{current = Now,
                           rcv_next = RcvNext,rx_rto = RxRto} = PCB)
  when ?DIFF_32(SndNext,Limit) < 0 ->
  Seg = aikcp_queue:front(SndQ),
  SndQ2 = aikcp_queue:pop_front(SndQ),
  Seg1 = Seg#aikcp_seg{cmd = ?KCP_CMD_PUSH, sn = SndNext,una = RcvNext,
                      ts = Now, resendts = Now, rto = RxRto},
  SndBuf2 = aikcp_buffer:append(Seg1,SndBuf),
  case aikcp_queue:size(SndQ2) > 0 of
    true -> queue_to_buffer(?BIT_32(SndNext + 1),Limit,SndQ2,SndBuf2,Wnd,PCB);
    false ->
      PCB#aikcp_pcb{snd_next = ?BIT_32(SndNext + 1),snd_queue = SndQ2,snd_buf = SndBuf2}
  end;
queue_to_buffer(SndNext,_,SndQ,SndBuf,_Wnd,PCB)->
  PCB#aikcp_pcb{snd_next = SndNext,snd_queue = SndQ,snd_buf = SndBuf}.


update_probe(#aikcp_pcb{current = Now,ts_probe = TSProbe,
                        probe_wait = ProbeWait,probe = Probe} = PCB)->
  if ProbeWait == 0 ->
      PCB#aikcp_pcb{probe_wait = ?KCP_PROBE_INIT,ts_probe = Now + ?KCP_PROBE_INIT};
     Now - TSProbe >= 0 ->
      ProbeWait2 =
        if ProbeWait < ?KCP_PROBE_INIT -> ?KCP_PROBE_INIT + ?KCP_PROBE_INIT /2;
           true -> ProbeWait + ProbeWait / 2
        end,
      ProbeWait3 =
        if ProbeWait2 > ?KCP_PROBE_LIMIT -> ?KCP_PROBE_LIMIT;
           true -> ProbeWait2
        end,
      PCB#aikcp_pcb{ts_probe = Now + ProbeWait3,
                    probe_wait = ProbeWait3,
                    probe = Probe bor ?KCP_ASK_SEND};
     true -> PCB
  end.
flush_probe(Wnd,Buffer,
            #aikcp_pcb{conv = Conv,rcv_next = Una,
                       probe = Probe,mtu = MTU} = PCB)
  when (Probe band ?KCP_ASK_SEND) == ?KCP_ASK_SEND->
  Bin = ?KCP_SEG(Conv, ?KCP_CMD_WASK, 0, Wnd, 0, 0, Una, 0, <<>>, <<>>),
  Buffer2 = build_buffer(Bin,Buffer,MTU),
  flush_probe(Wnd,Buffer2,PCB#aikcp_pcb{probe = Probe bxor ?KCP_ASK_SEND});
flush_probe(Wnd,Buffer,
            #aikcp_pcb{conv = Conv,rcv_next = Una,
                       probe = Probe,mtu = MTU} = PCB)
  when (Probe band ?KCP_ASK_TELL) == ?KCP_ASK_TELL->
  Bin = ?KCP_SEG(Conv, ?KCP_CMD_WINS, 0, Wnd, 0, 0, Una, 0, <<>>, <<>>),
  Buffer2 = build_buffer(Bin,Buffer,MTU),
  flush_probe(Wnd,Buffer2,PCB#aikcp_pcb{probe = Probe bxor ?KCP_ASK_TELL});
flush_probe(_Wnd,Buffer,PCB)-> {Buffer,PCB#aikcp_pcb{probe = 0}}.



flush_ack(Wnd, #aikcp_pcb{conv = Conv,rcv_next = RcvNext,
                           acklist = AckList, mtu = MTU} = PCB) ->
  Buffer = flush_ack(Conv,Wnd,RcvNext, AckList, [], MTU),
  {Buffer,PCB#aikcp_pcb{acklist = [],ackcount = 0}}.

flush_ack(_Conv,_Wnd,_Una, [], Buffer, _Limit) -> Buffer;
flush_ack(Conv,Wnd,Una,[{Sn, Ts} | Left], Buffer, Limit) ->
  Bin = ?KCP_SEG(Conv, ?KCP_CMD_ACK, 0, Wnd, Ts, Sn, Una, 0, <<>>, <<>>),
  Buffer2 = build_buffer(Bin,Buffer,Limit),
  flush_ack(Conv,Wnd,Una,Left,Buffer2,Limit).

build_buffer(Bin,[],_Limit)-> [Bin];
build_buffer(Bin,[H|T],Limit) ->
  BinSize = byte_size(Bin),
  HeadSize = byte_size(H),
  if (BinSize + HeadSize + ?KCP_OVERHEAD) > Limit -> [Bin,H|T];
     true ->
      Buffer = aikcp_util:join([Bin,H]),
      [Buffer | T]
  end.

% 自身窗口(可接收数据)
wnd(#aikcp_pcb{rcv_wnd = RcvWnd, rcv_queue = RcvQ})->
  Diff = RcvWnd - aikcp_queue:size(RcvQ),
  if Diff > 0 -> Diff;
     true -> 0
  end.
% 远程窗口(可发送数据)
pwnd(#aikcp_pcb{snd_wnd = SndWnd,rmt_wnd = RmtWnd,
                cwnd = CWnd,nocwnd = NoCWnd})->
  PWnd = ?MIN(SndWnd,RmtWnd), % 发送窗口和远程窗口取最小
  if NoCWnd == 0 -> ?MIN(CWnd,PWnd);
     true -> PWnd
  end.



