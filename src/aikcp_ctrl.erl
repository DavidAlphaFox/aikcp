-module(aikcp_ctrl).
-compile({inline,[split/2,queue_send/2]}).

-include("aikcp.hrl").
-export([send/2,new/1,update/1]).


new(Conv)-> #aikcp_pcb{conv = Conv}.
update(PCB) -> update(aikcp_util:millisecond(),PCB).

update(Now,#aikcp_pcb{updated = true,ts_flush = TSFlush,interval = Interval} = PCB) ->
  Slap = Now - TSFlush,
  {Slap2, TSFlush2} =
    case (Slap >= 10000) or (Slap < -10000) of
      true -> {0, Now};
      false -> {Slap, TSFlush}
    end,
  case Slap2 >= 0 of
    false -> PCB#aikcp_pcb{current = Now, ts_flush = TSFlush2};
    true  ->
      TSFlush3 = TSFlush + Interval,
      TSFlush4 =
        if Now > TSFlush3 -> Now + Interval;
           true -> TSFlush3
        end,
      flush(PCB#aikcp_pcb{current = Now, ts_flush = TSFlush4})
  end;

update(Now,PCB) -> update(Now,PCB#aikcp_pcb{ts_flush = Now,updated = true}).

flush(#aikcp_pcb{ackcount = AckCount,rmt_wnd = RmtWnd,
                snd_next = SndNext,snd_una = SndUna} = PCB)->
  Wnd =  wnd(PCB), %% 自己的窗口
  {Buffer,PCB2} =
    if AckCount > 0 -> flush_ack(Wnd,PCB);
       true -> {[],PCB}
    end,
  PCB3 =
    if RmtWnd == 0 -> update_probe(PCB2);
       true -> PCB#aikcp_pcb{probe_wait = 0,ts_probe = 0}
    end,
  {Buffer2,PCB4} =  flush_probe(Wnd,Buffer,PCB3),
  PWnd = pwnd(PCB4), %% 对方的窗口
  CanSend =  SndUna + PWnd - SndNext,
  PCB5 =
    if CanSend > 0 -> flush_data(Wnd,PWnd,PCB4);
       true ->  PCB4
    end,
  {Buffer3,PCB6} = flush_resend(Wnd,PWnd,Buffer2,PCB5),
  {lists:reverse(Buffer3),PCB6}.


flush_resend(Wnd,PWnd,Buffer,
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
  do_resend(Wnd,PWnd,ReSent,RtoMin,Buffer,
            aikcp_buffer:head(SndBuf),false,false,PCB).
do_resend(_Wnd,PWnd,ReSent,_RtoMin,Buffer,-1,Change,Lost,PCB)->
  PCB2 =
    case Change of
      false -> PCB;
      true  ->
        #aikcp_pcb{snd_next = SndNext, snd_una = SndUna, mss = MSS} = PCB,
        Thresh2 =
          case (SndNext - SndUna) div 2 of
            V when V < ?KCP_THRESH_MIN -> ?KCP_THRESH_MIN;
            V -> V
          end,
        CWnd = Thresh2 + ReSent,
        PCB#aikcp_pcb{cwnd = CWnd,incr = CWnd * MSS, ssthresh = Thresh2}
    end,
  PCB3 =
    if Lost == true -> PCB#aikcp_pcb{ssthresh = ?MAX(?KCP_THRESH_MIN, PWnd div 2),
                                     cwnd = 1, incr = PCB2#aikcp_pcb.mss};
       true -> PCB2
    end,
  PCB4 =
    if PCB3#aikcp_pcb.cwnd < 1 ->
        PCB3#aikcp_pcb{cwnd = 1,incr = PCB3#aikcp_pcb.mss};
       true -> PCB3
    end,
  {Buffer,PCB4};

do_resend(Wnd,PWnd,ReSent,RtoMin,Buffer,Idx,Change,Lost,
         #aikcp_pcb{current = Now,snd_buf = SndBuf,nodelay = NoDelay,
                    rx_rto = Rto,mtu = MTU, fastlimit = FastLimit,
                    rcv_next = RcvNext,xmit = PCBXmit} = PCB)->
  Next = aikcp_buffer:next(Idx, SndBuf),
  case aikcp_buffer:data(Idx,SndBuf) of
    undefined -> do_resend(Wnd,PWnd,ReSent,RtoMin,Buffer,Next,Change,Lost,PCB);
    #aikcp_seg{xmit = Xmit, resendts = ReSentTS,
               fastack = FastAck, rto = SegRto} = Seg ->
      {NeedSend,Lost2,Change2,Seg2,XmitAcc} =
        if Xmit == 0 ->
            {true,Lost,Change,Seg#aikcp_seg{xmit = Xmit + 1, rto = Rto,
                                            resendts = Now + Rto + RtoMin},0};
           Now >= ReSentTS->
            SegRto2 =
              if NoDelay == 0 -> SegRto + ?MAX(Rto,SegRto);
                 NoDelay < 2 -> SegRto + SegRto div 2;
                 true -> SegRto + Rto div 2
              end,
            {true,true,Change, Seg#aikcp_seg{xmit = Xmit + 1,rto = SegRto2,
                                             resendts = Now + SegRto2},1};
           (FastAck >= ReSent)
           and ((Xmit < FastLimit)  or (FastLimit =< 0))->
            {true, Lost, true, Seg#aikcp_seg{xmit = Xmit + 1, fastack = 0,
                                             resendts = Now + SegRto},0};
           true -> {false, Lost, Change, Seg,0}
        end,

        if NeedSend == true ->
            #aikcp_seg{conv = Conv, frg = Frg,
                       sn = Sn, len = Len, data = Data} = Seg2,
            Bin = ?KCP_SEG(Conv, ?KCP_CMD_PUSH, Frg, Wnd, Now, Sn,RcvNext, Len, Data, <<>>),
            Buffer2 = build_buffer(Bin,Buffer,MTU),
            SndBuf2 = aikcp_buffer:replace(Idx,Seg2#aikcp_seg{ts = Now,una = RcvNext}, SndBuf),
            do_resend(Wnd,PWnd,ReSent,RtoMin,Buffer2,Next,Change2,Lost2,
                      PCB#aikcp_pcb{snd_buf = SndBuf2,xmit = PCBXmit + XmitAcc});
           true -> do_resend(Wnd,PWnd,ReSent,RtoMin,Buffer,Next,Change2,Lost2,PCB)
        end
  end.





flush_data(Wnd,PWnd,#aikcp_pcb{snd_queue = SndQ, snd_buf = SndBuf,
                               snd_next = SndNext, snd_una = SndUna} = PCB)->
  case aikcp_queue:size(SndQ) == 0 of
    true -> PCB;
    false -> queue_to_buffer(SndNext,SndUna + PWnd,SndQ,SndBuf,Wnd,PCB)
  end.

queue_to_buffer(SndNext,Limit,SndQ,SndBuf,_Wnd,PCB) when SndNext >= Limit->
  PCB#aikcp_pcb{snd_next = SndNext,snd_queue = SndQ,snd_buf = SndBuf};
queue_to_buffer(SndNext,Limit,SndQ,SndBuf,Wnd,
                #aikcp_pcb{current = Now,
                           rcv_next = RcvNext,rx_rto = RxRto} = PCB)->
  Seg = aikcp_queue:front(SndQ),
  SndQ2 = aikcp_queue:pop_front(SndQ),
  Seg1 = Seg#aikcp_seg{cmd = ?KCP_CMD_PUSH, sn = SndNext,una = RcvNext,
                      ts = Now, resendts = Now, rto = RxRto},
  SndBuf2 = aikcp_buffer:append(Seg1,SndBuf),
  queue_to_buffer(SndNext + 1,Limit,SndQ2,SndBuf2,Wnd,PCB).



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
flush_probe(Wnd,Buffer,#aikcp_pcb{conv = Conv,rcv_next = Una,probe = Probe,mtu = MTU} = PCB)
  when (Probe band ?KCP_ASK_SEND) == ?KCP_ASK_SEND->
  Bin = ?KCP_SEG(Conv, ?KCP_CMD_WASK, 0, Wnd, 0, 0, Una, 0, <<>>, <<>>),
  Buffer2 = build_buffer(Bin,Buffer,MTU),
  flush_probe(Wnd,Buffer2,PCB#aikcp_pcb{probe = Probe bxor ?KCP_ASK_SEND});
flush_probe(Wnd,Buffer,#aikcp_pcb{conv = Conv,rcv_next = Una,probe = Probe,mtu = MTU} = PCB)
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
      Buffer = join([Bin,H]),
      [Buffer | T]
  end.



wnd(#aikcp_pcb{rcv_wnd = RcvWnd, rcv_queue = RcvQ})->
  Diff = RcvWnd - aikcp_queue:size(RcvQ),
  if Diff > 0 -> Diff;
     true -> 0
  end.
pwnd(#aikcp_pcb{snd_wnd = SndWnd,rmt_wnd = RmtWnd,
                cwnd = CWnd,nocwnd = NoCWnd})->
  PWnd = ?MIN(SndWnd,RmtWnd),
  if NoCWnd == 0 -> ?MIN(CWnd,PWnd);
     true -> PWnd
  end.



send(<<>>,PCB)-> PCB;
send(Binary,#aikcp_pcb{stream = true,snd_queue = SndQ,mss = MSS} = PCB)->
    case aikcp_queue:empty(SndQ) of
      true -> queue_send(Binary,PCB);
      false ->
        Seg = aikcp_queue:back(SndQ),
        Diff = MSS - Seg#aikcp_seg.len,
        if Diff > 0 ->
            <<Fill:Diff/bytes,Left/binary>> = Binary,
            Data = Seg#aikcp_seg.data,
            Data2 = <<Data/binary,Fill/binary>>,
            Seg2 = Seg#aikcp_seg{data = Data2, len = byte_size(Data2)},
            SndQ2 = aikcp_queue:push_back(Seg2,aikcp_queue:pop_back(SndQ)),
            queue_send(Left,PCB#aikcp_pcb{snd_queue = SndQ2});
           true -> queue_send(Binary,PCB)
        end
    end;
send(Binary,#aikcp_pcb{mss = MSS})
  when byte_size(Binary) >= MSS * 128->
  {error,fragment_overflow};
send(Binary,PCB) -> queue_send(Binary,PCB).

queue_send(Binary,#aikcp_pcb{mss = MSS} = PCB)->
  Payloads = split(Binary,MSS),
  queue_send(Payloads,length(Payloads),PCB).

queue_send([], 0,PCB) -> PCB;
queue_send( [Data | Left], Frg,#aikcp_pcb{stream = Stream,snd_queue = SndQue} = PCB) ->
  %% 流模式下无分片
  Frg2 =
    if Stream =:= true -> 0;
       true -> Frg -1
    end,
  Seg = #aikcp_seg{len = byte_size(Data), frg = Frg2, data = Data},
  queue_send(Left, Frg - 1,PCB#aikcp_pcb{snd_queue = aikcp_queue:push_back(Seg, SndQue)}).


split(Data,Len) when byte_size(Data) =< Len -> [Data];
split(Data,Len) -> split2(Len, Data, []).

split2(_, <<>>, Rslt) -> lists:reverse(Rslt);
split2(Len, Data, Rslt) ->
  case Data of
    <<Head:Len/bytes, Left/binary>> -> split2(Len, Left, [Head | Rslt]);
    _ -> split2(Len, <<>>, [Data | Rslt])
  end.

join([]) -> <<>>;
join([Part]) -> Part;
join(List) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).
