-module(aikcp_pcb).
-compile({inline,[split/2,queue_send/2]}).

-include("aikcp.hrl").
-export([send/2,
         recv/2,
         new/1,
         update/1]).


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
    false -> {[],PCB#aikcp_pcb{current = Now, ts_flush = TSFlush2}};
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
  lists:foldr(
    fun (I, Acc) ->  <<Acc/binary,I/binary>> end,
    <<>>, List).

recv(Binary,PCB)->
  PCB2 = recv(Binary,{undefined,undefined},PCB),
  check_recv(PCB2).

check_recv(#aikcp_pcb{rcv_queue = RcvQ,probe = Probe,
                rcv_next = RcvNext, rcv_buf = RcvBuf,
                rcv_wnd = RcvWnd} = PCB)->
  case aikcp_queue:empty(RcvQ) of
    true -> {undefined,PCB};
    false ->
      {RcvQ2, Payload} = queue_to_binary(RcvQ, [], []),
      {RcvNext2, RcvBuf2, RcvQ3} = buffer_to_queue(RcvNext, RcvBuf, RcvQ2),
      Probe2 =
        case (aikcp_queue:size(RcvQ) >= RcvWnd)
           and (aikcp_queue:size(RcvQ3) < RcvWnd) of
          true ->Probe bor ?KCP_ASK_TELL;
          false -> Probe
        end,
      {Payload,PCB#aikcp_pcb{rcv_next = RcvNext2, rcv_queue = RcvQ3,
                             rcv_buf = RcvBuf2, probe = Probe2}}
  end.

queue_to_binary(RcvQ, PartList, DataList) ->
  case aikcp_queue:empty(RcvQ) of
    true  -> {RcvQ, join(DataList)};
    false ->
      case aikcp_queue:front(RcvQ) of
        #aikcp_seg{frg = Frg, data = Data} when Frg =:= 0 ->
          Data2 = join([Data | PartList]),
          RcvQ2 = aikcp_queue:pop_front(RcvQ),
          queue_to_binary(RcvQ2, [], [Data2 | DataList]);
        #aikcp_seg{frg = Frg, data = Data} ->
          case aikcp_queue:size(RcvQ) of
            QueSize when QueSize >= (Frg + 1) ->
              RcvQ2 = aikcp_queue:pop_front(RcvQ),
              queue_to_binary(RcvQ2, [Data | PartList], DataList);
            _ -> {RcvQ, join(DataList)}
          end
      end
  end.


recv(<<>>,{MaxAck,Una},PCB)-> recv_finish(MaxAck,Una,PCB);
recv(?KCP_SEG(_Conv, ?KCP_CMD_ACK, 0, _Wnd, Ts, Sn, Una, 0, _Data, Left),
     {MaxAck,_},#aikcp_pcb{current = Now} = PCB)->
  PCB2 = una(Una,PCB),
  PCB3 = shrink_sndbuf(PCB2),
  RTT = Now - Ts,
  PCB4 =
    if RTT >= 0 -> update_rtt(RTT,PCB3);
       true -> PCB3
    end,
  PCB5 = ack(Sn,PCB4),
  PCB6 = shrink_sndbuf(PCB5),
  MaxAck2 =
    if MaxAck == undefined -> Sn;
       Sn > MaxAck -> Sn;
       true -> MaxAck
    end,
  recv(Left,{MaxAck2, Una},PCB6);
recv(?KCP_SEG(Conv, ?KCP_CMD_PUSH, Frg, Wnd, Ts, Sn, Una, Len, Data, Left),
     {MaxAck,_},#aikcp_pcb{rcv_wnd = RcvWnd,
                           rcv_next = RcvNext} = PCB)->
  PCB2 = una(Una,PCB),
  PCB3 = shrink_sndbuf(PCB2),
  if Sn < (RcvWnd + RcvNext) ->
      PCB4 = append_acklist(Sn,Ts,PCB3),
      if Sn >= RcvNext ->
          Seg = #aikcp_seg{conv = Conv, cmd = ?KCP_CMD_PUSH, wnd = Wnd,
                           frg = Frg, ts = Ts, sn = Sn, una = Una,
                           len = Len, data = Data},
          append_data(Seg,PCB4);
         true -> PCB4
      end;
     true -> recv(Left,{MaxAck,Una},PCB3)
  end;
recv(?KCP_SEG(_Conv, ?KCP_CMD_WASK, 0, Wnd, _Ts, _Sn, Una, 0, _Data, Left),
     {MaxAck, _},#aikcp_pcb{probe = Probe } = PCB) ->
  PCB2 = una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  recv(Left,{MaxAck,Una},PCB3#aikcp_pcb{probe = Probe bor ?KCP_ASK_TELL});
recv(?KCP_SEG(_Conv, ?KCP_CMD_WINS, 0, Wnd, _Ts, _Sn, Una, 0, _Data, Left),
     {MaxAck, _}, PCB) ->
  PCB2 = una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  recv(Left,{MaxAck,Una},PCB3).

append_data(#aikcp_seg{sn = Sn},#aikcp_pcb{rcv_next = RcvNext, rcv_wnd = Rwnd} = PCB)
    when Sn >= RcvNext + Rwnd; Sn < RcvNext -> PCB;
append_data(Seg,#aikcp_pcb{rcv_buf = RcvBuf, rcv_queue = RcvQ, rcv_next = RcvNext} = PCB) ->
  Idx = aikcp_buffer:head(RcvBuf),
  RcvBuf2 = append_data(Seg, Idx, -1,RcvBuf),
  {RcvNext2, RcvBuf3, RcvQ2} = buffer_to_queue(RcvNext, RcvBuf2, RcvQ),
  PCB#aikcp_pcb{rcv_buf = RcvBuf3, rcv_queue = RcvQ2, rcv_next = RcvNext2}.

append_data(Seg, -1, Prev,RcvBuf) -> aikcp_buffer:insert(Prev, Seg, RcvBuf);
append_data(Seg, Idx, Prev, RcvBuf) ->
  Next = aikcp_buffer:next(Idx, RcvBuf),
  case aikcp_buffer:data(Idx, RcvBuf) of
    undefined -> RcvBuf;
    #aikcp_seg{sn = Sn} when Sn =:= Seg#aikcp_seg.sn -> RcvBuf;
    #aikcp_seg{sn = Sn} when Sn < Seg#aikcp_seg.sn -> append_data(Seg, Next, Idx, RcvBuf);
    #aikcp_seg{sn = Sn} when Sn > Seg#aikcp_seg.sn -> aikcp_buffer:insert(Prev, Seg, RcvBuf)
  end.

buffer_to_queue(RcvNext, RcvBuf, RcvQ) ->
  Idx = aikcp_buffer:head(RcvBuf),
  buffer_to_queue(RcvNext,Idx,-1, RcvBuf, RcvQ).

buffer_to_queue(RcvNext,-1,_, RcvBuf, RcvQ) ->{RcvNext, RcvBuf, RcvQ};
buffer_to_queue(RcvNext,Idx, Prev, RcvBuf, RcvQ) ->
  case aikcp_buffer:data(Idx, RcvBuf) of
    undefined -> {RcvNext, RcvBuf, RcvQ};
    #aikcp_seg{sn = Sn} = Seg when Sn =:= RcvNext ->
      Next = aikcp_buffer:next(Idx, RcvBuf),
      RcvBuf2 = aikcp_buffer:delete(Idx, Prev, RcvBuf),
      RcvQ2 = aikcp_queue:push_back(Seg, RcvQ),
      Prev2 =
        case aikcp_buffer:head(RcvBuf2) == Next of
          true -> -1;
          false -> Idx
        end,
      buffer_to_queue(RcvNext + 1,Next,Prev2,RcvBuf2,RcvQ2);
    _ -> {RcvNext, RcvBuf, RcvQ}
  end.

append_acklist(Sn, Ts, #aikcp_pcb{acklist = AckList,ackcount = AckCount} = PCB) ->
  PCB#aikcp_pcb{acklist = [{Sn, Ts} | AckList],ackcount = AckCount + 1}.

recv_finish(MaxAck,Una,PCB) ->
  PCB2 =
    if MaxAck =:= undefined -> PCB;
       true -> fastack(MaxAck,PCB)
    end,
  recv_finish(Una,PCB2).

recv_finish(Una,#aikcp_pcb{snd_una = SndUna, cwnd = Cwnd, rmt_wnd = Rwnd}= PCB)
  when SndUna =< Una; Cwnd >= Rwnd -> PCB;
recv_finish(_Una,#aikcp_pcb{mss = MSS, cwnd = Cwnd, incr = Incr,
                            ssthresh = Ssth, rmt_wnd = Rwnd} = PCB) ->
  if
    Cwnd >= Rwnd -> PCB;
    true ->
      {Cwnd2, Incr2} =
        if Cwnd < Ssth -> {Cwnd + 1, Incr + MSS};
           true ->
            In2 = ?MAX(Incr, MSS),
            In3 = In2 + (MSS * MSS) div In2 + (MSS div 16),
            DivMSS =
              if MSS > 0 -> MSS;
                 true -> 1
              end,
            C2 =
              if (Cwnd + 1) * MSS =< In3 -> (In3 + MSS - 1) / DivMSS;
                 true -> Cwnd
              end,
            {C2, In3}
        end,
      if Cwnd2 > Rwnd -> PCB#aikcp_pcb{cwnd = Rwnd, incr = Rwnd * MSS};
         true -> PCB#aikcp_pcb{cwnd = Cwnd2, incr = Incr2}
      end
  end.


%% Increase FaskAck count for each segment
fastack(Sn,#aikcp_pcb{snd_una = SndUna, snd_next = SndNext} = PCB )
    when SndUna > Sn; Sn >= SndNext -> PCB;
fastack(Sn,#aikcp_pcb{snd_buf = SndBuf} = PCB) ->
  Idx = aikcp_buffer:head(SndBuf),
  SndBuf2 = fastack(Sn,Idx, SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.

fastack(_,-1, SndBuf) -> SndBuf;
fastack(Sn, Idx, SndBuf) ->
  case aikcp_buffer:data(Idx, SndBuf) of
    undefined -> fastack(Sn,aikcp_buffer:next(SndBuf, Idx),SndBuf);
    Seg when Seg#aikcp_seg.sn > Sn -> fastack(Sn, -1, SndBuf);
    #aikcp_seg{fastack = FastAck}  = Seg->
      Seg2 = Seg#aikcp_seg{fastack = FastAck + 1},
      SndBuf2 = aikcp_buffer:replace(Idx, Seg2, SndBuf),
      Next = aikcp_buffer:next(Idx,SndBuf2),
      fastack(Sn,Next,SndBuf2)
  end.

ack(Sn,#aikcp_pcb{snd_una = SndUna,snd_next = SndNext} = PCB)
  when SndUna > Sn; SndNext =< Sn -> PCB;
ack(Sn,#aikcp_pcb{snd_buf = SndBuf} = PCB) ->
  Idx = aikcp_buffer:head(SndBuf),
  SndBuf2 = ack(Sn, Idx, -1,SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.
ack(_, -1, _, SndBuf) -> SndBuf;
ack(Sn, Idx, Prev, SndBuf) ->
  {Next2, SndBuf2} =
    case aikcp_buffer:data(Idx, SndBuf) of
      undefined -> {aikcp_buffer:next(Idx, SndBuf), SndBuf};
      Seg when Seg#aikcp_seg.sn =:= Sn -> {-1, aikcp_buffer:delete(Idx, Prev, SndBuf)};
      Seg when Seg#aikcp_seg.sn > Sn -> {-1, SndBuf};
      Seg when Seg#aikcp_seg.sn < Sn -> {aikcp_buffer:next(Idx, SndBuf), SndBuf}
    end,
  ack(Sn,Next2,Idx,SndBuf2).
  
%% update kcp rx_rttval, rx_srtt and rx_rto
update_rtt(RTT,PCB) when RTT < 0 -> PCB;
update_rtt(RTT,PCB) ->
  PCB2 =
    if PCB#aikcp_pcb.rx_srtt == 0 ->
        PCB#aikcp_pcb{rx_srtt = RTT, rx_rttval = RTT div 2};
    true ->
        Delta = abs(RTT - PCB#aikcp_pcb.rx_srtt),
        RxRttVal = (3 * PCB#aikcp_pcb.rx_rttval + Delta) div 4,
        RxSRttVal = (7 * PCB#aikcp_pcb.rx_srtt + RTT) div 8,
        RxSRttVal2 =
          if RxSRttVal < 1 -> 1;
             true -> RxSRttVal
          end,
        PCB#aikcp_pcb{rx_srtt = RxSRttVal2, rx_rttval = RxRttVal}
    end,
  Rto = PCB2#aikcp_pcb.rx_srtt + ?MAX(PCB2#aikcp_pcb.interval, 4 * PCB2#aikcp_pcb.rx_rttval),
  Rto2 = aikcp_util:clamp(Rto,PCB2#aikcp_pcb.rx_minrto,?KCP_RTO_MAX),
  PCB2#aikcp_pcb{rx_rto = Rto2}.

una(Una,#aikcp_pcb{snd_buf = SndBuf} = PCB)->
  Idx = aikcp_buffer:head(SndBuf),
  SndBuf2 = una(Una,Idx,-1,SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.
una(_Una,-1,_,SndBuf) -> SndBuf;
una(Una,Idx,Prev,SndBuf) ->
  {Next2,SndBuf2} =
    case aikcp_buffer:data(Idx,SndBuf) of
      undefined -> {aikcp_buffer:next(Idx,SndBuf),SndBuf};
      Seg when Seg#aikcp_seg.sn >= Una -> {-1,SndBuf};
      _ ->
        Next = aikcp_buffer:next(Idx, SndBuf),
        Buf2 = aikcp_buffer:delete(Idx, Prev, SndBuf),
        {Next, Buf2}
    end,
  una(Una,Next2,Idx,SndBuf2).

shrink_sndbuf(#aikcp_pcb{snd_buf = SndBuf, snd_next = SndNext} = PCB) ->
  case aikcp_buffer:head(SndBuf) of
    -1 -> PCB#aikcp_pcb{snd_una = SndNext};
    Idx ->
      Seg = aikcp_buffer:data(Idx, SndBuf),
      PCB#aikcp_pcb{snd_una = Seg#aikcp_seg.sn}
  end.
