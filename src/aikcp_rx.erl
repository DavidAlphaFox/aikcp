%% 接收数据
-module(aikcp_rx).
-include("aikcp.hrl").
-export([handle/2,
         recv/1]).

-record(rx, {max_ack = undefined,
             prev_una = undefined,
             flag = 0,
             latest_ts = 0}).

recv(#aikcp_pcb{rcv_queue = RcvQ,probe = Probe,
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
    true  -> {RcvQ, aikcp_util:join(DataList)};
    false ->
      case aikcp_queue:front(RcvQ) of
        #aikcp_seg{frg = Frg, data = Data} when Frg == 0 ->
          Data2 = aikcp_util:join([Data | PartList]),
          RcvQ2 = aikcp_queue:pop_front(RcvQ),
          queue_to_binary(RcvQ2, [], [Data2 | DataList]);
        #aikcp_seg{frg = Frg, data = Data} ->
          case aikcp_queue:size(RcvQ) of
            QueSize when QueSize >= (Frg + 1) ->
              RcvQ2 = aikcp_queue:pop_front(RcvQ),
              queue_to_binary(RcvQ2, [Data | PartList], DataList);
            _ -> {RcvQ, aikcp_util:join(DataList)}
          end
      end
  end.

handle(Binary,PCB)->
  process(Binary,
         #rx{prev_una = PCB#aikcp_pcb.snd_una},
          PCB).
process(<<>>,Acc,PCB) -> done(Acc,PCB);   
process(Binary,Acc,PCB)
  when erlang:byte_size(Binary) < ?KCP_OVERHEAD ->
  done(Acc,PCB);
process(?KCP_SEG(_Conv, ?KCP_CMD_ACK, _Frag, Wnd, Ts, Sn, Una, _Len, _Data, Left),
        Acc,#aikcp_pcb{current = Now} = PCB)->
  MaxAck = Acc#rx.max_ack,
  Flag = Acc#rx.flag,
  PCB2 = process_una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  RTT = Now - Ts,
  PCB4 =
    if RTT >= 0 -> aikcp_rtt:handle(RTT, PCB3);
       true -> PCB3
    end,
  PCB5 = process_ack(Sn,PCB4),
  PCB6 = shrink_sndbuf(PCB5),
  Acc1 =
    if Flag == 0 ->
        Acc#rx{flag = 1,latest_ts = Ts,max_ack = MaxAck};
       true ->
        if ?DIFF_32(Sn,MaxAck) > 0 ->
            Acc#rx{latest_ts = Ts,max_ack = MaxAck};
          true -> Acc
        end
    end,
  process(Left,Acc1,PCB6);
process(?KCP_SEG(Conv, ?KCP_CMD_PUSH, Frg, Wnd, Ts, Sn, Una, Len, Data, Left),
        Acc,#aikcp_pcb{rcv_wnd = RcvWnd,ackcount = AckCount,acklist = AckList,
                       rcv_next = RcvNext} = PCB)->
  PCB2 = process_una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  Diff = ?DIFF_32(Sn,?BIT_32(RcvWnd + RcvNext)),
  if Diff < 0 ->
      PCB4 =  PCB3#aikcp_pcb{acklist = [{Sn, Ts} | AckList],
                             ackcount = AckCount + 1},
      Diff1 = ?DIFF_32(Sn,RcvNext),
      PCB5 =
        if Diff1 >= 0 ->
            Seg = #aikcp_seg{conv = Conv, cmd = ?KCP_CMD_PUSH, wnd = Wnd,
                             frg = Frg, ts = Ts, sn = Sn, una = Una,
                             len = Len, data = Data},
            process_data(Seg,PCB4);
           true -> PCB4
        end,
      process(Left,Acc,PCB5);
     true -> process(Left,Acc,PCB3)
  end;
process(?KCP_SEG(_Conv, ?KCP_CMD_WASK, _Frag, Wnd, _Ts, _Sn, Una, _Len, _Data, Left),
        Acc,#aikcp_pcb{probe = Probe } = PCB) ->
  PCB2 = process_una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  process(Left,Acc,PCB3#aikcp_pcb{probe = Probe bor ?KCP_ASK_TELL});
process(?KCP_SEG(_Conv, ?KCP_CMD_WINS,_Frag, Wnd, _Ts, _Sn, Una, _Len, _Data, Left),
     Acc, PCB) ->
  PCB2 = process_una(Una,PCB#aikcp_pcb{rmt_wnd = Wnd}),
  PCB3 = shrink_sndbuf(PCB2),
  process(Left,Acc,PCB3);
process(?KCP_SEG(_Conv,_Cmd, _S1, _Wnd, _Ts, _Sn, _Una, _S2, _Data, Left),
       Acc,PCB)->
  process(Left,Acc,PCB).
done(#rx{flag = Flag,
         max_ack = MaxAck,
         latest_ts = Ts} = Acc,PCB) ->
  PCB2 =
    if Flag == 0 -> PCB;
       true -> process_fastack(MaxAck,Ts,PCB)
    end,
  done_1(Acc,PCB2).

done_1(#rx{prev_una = Una},
            #aikcp_pcb{snd_una = SndUna, cwnd = Cwnd, rmt_wnd = Rwnd}= PCB)
  when ?DIFF_32(Una, SndUna) >= 0 ; Cwnd >= Rwnd -> PCB;
done_1(_,#aikcp_pcb{mss = MSS, cwnd = Cwnd, incr = Incr,
                    ssthresh = Ssth, rmt_wnd = Rwnd} = PCB) ->
  PCB1 =
    if Cwnd < Ssth ->
        PCB#aikcp_pcb{cwnd = Cwnd + 1,incr = Incr + MSS};
       true ->
        Incr2 =
          if Incr < Ssth -> Incr + MSS;
             true -> Incr
          end,
        Incr3 = Incr2 + (MSS * MSS) div Incr2 + MSS div 16,
        if (Cwnd + 1 ) * MSS =< Incr3 ->
            PCB#aikcp_pcb{cwnd = (Incr3 + MSS - 1) div MSS,
                          incr = Incr3};
           true ->
            PCB#aikcp_pcb{incr = Incr3}
        end
    end,
  if
    Cwnd > Rwnd ->
      PCB1#aikcp_pcb{cwnd = Rwnd,incr = Rwnd * MSS};
    true -> PCB1
  end.


process_fastack(Sn,_,
                #aikcp_pcb{snd_una = SndUna,snd_next = SndNext} =PCB)
  when ?DIFF_32(Sn, SndUna) < 0 ;
       ?DIFF_32(Sn, SndNext) >=0 -> PCB;
process_fastack(Sn,_,#aikcp_pcb{snd_buf = SndBuf} = PCB) ->
  Idx = aikcp_buffer:head(SndBuf),
  SndBuf2 = fastack(Sn,Idx, SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.

fastack(_,-1, SndBuf) -> SndBuf;
fastack(Sn, Idx, SndBuf) ->
  case aikcp_buffer:data(Idx, SndBuf) of
    Seg when ?DIFF_32(Seg#aikcp_seg.sn, Sn) >= 0 -> fastack(Sn, -1, SndBuf);
    #aikcp_seg{fastack = FastAck}  = Seg->
      Seg2 = Seg#aikcp_seg{fastack = FastAck + 1},
      SndBuf2 = aikcp_buffer:replace(Idx, Seg2, SndBuf),
      Next = aikcp_buffer:next(Idx,SndBuf2),
      fastack(Sn,Next,SndBuf2)
  end.

%% UNA（此编号前所有包已收到，如TCP）
process_una(Una,#aikcp_pcb{snd_buf = SndBuf} = PCB)->
  Idx = aikcp_buffer:head(SndBuf), %% 获取发送buffer第一个包
  SndBuf2 = una(Una,Idx,-1,SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.

una(_Una,-1,_,SndBuf) -> SndBuf;
una(Una,Idx,Prev,SndBuf) ->
  {Next2,SndBuf2} =
    case aikcp_buffer:data(Idx,SndBuf) of
      undefined -> {aikcp_buffer:next(Idx,SndBuf),SndBuf};
      Seg when ?DIFF_32(Seg#aikcp_seg.sn, Una) >= 0 ->  {-1,SndBuf};
      _ ->
        Next = aikcp_buffer:next(Idx, SndBuf),
        Buf2 = aikcp_buffer:delete(Idx, Prev, SndBuf),
        {Next, Buf2}
    end,
  una(Una,Next2,Idx,SndBuf2).
%% 收到的包的号码小于UNA的话说明是重复包，大于SndNext可能是攻击或重复包
process_ack(Sn,#aikcp_pcb{snd_una = SndUna,snd_next = SndNext} = PCB)
  when ?DIFF_32(Sn, SndUna) < 0 ; ?DIFF_32(Sn, SndNext) >= 0 ->PCB;
process_ack(Sn,#aikcp_pcb{snd_buf = SndBuf} = PCB) ->
  Idx = aikcp_buffer:head(SndBuf),
  SndBuf2 = ack(Sn, Idx, -1,SndBuf),
  PCB#aikcp_pcb{snd_buf = SndBuf2}.
%% 遍历buffer处理包
ack(_, -1, _, SndBuf) -> SndBuf;
ack(Sn, Idx, Prev, SndBuf) ->
  {Next2, SndBuf2} =
    case aikcp_buffer:data(Idx, SndBuf) of
      undefined -> {aikcp_buffer:next(Idx, SndBuf), SndBuf};
      Seg when Seg#aikcp_seg.sn == Sn -> {-1, aikcp_buffer:delete(Idx, Prev, SndBuf)};
      Seg when ?DIFF_32(Seg#aikcp_seg.sn, Sn) > 0 -> {-1, SndBuf};
      _ -> {aikcp_buffer:next(Idx, SndBuf), SndBuf}
    end,
  ack(Sn,Next2,Idx,SndBuf2).

shrink_sndbuf(#aikcp_pcb{snd_buf = SndBuf, snd_next = SndNext} = PCB) ->
  case aikcp_buffer:head(SndBuf) of
    -1 -> PCB#aikcp_pcb{snd_una = SndNext};
    Idx ->
      Seg = aikcp_buffer:data(Idx, SndBuf),
      PCB#aikcp_pcb{snd_una = Seg#aikcp_seg.sn}
  end.

%% 收数据
process_data(#aikcp_seg{sn = Sn},#aikcp_pcb{rcv_next = RcvNext, rcv_wnd = Rwnd} = PCB)
  when ?DIFF_32(Sn, (RcvNext + Rwnd)) >= 0; ?DIFF_32(Sn,RcvNext) < 0 -> PCB;
process_data(Seg,
          #aikcp_pcb{rcv_buf = RcvBuf, rcv_queue = RcvQ,
                     rcv_next = RcvNext} = PCB) ->
  Idx = aikcp_buffer:head(RcvBuf),
  RcvBuf2 = data(Seg, Idx, -1,RcvBuf),
  {RcvNext2, RcvBuf3, RcvQ2} = buffer_to_queue(RcvNext, RcvBuf2, RcvQ),
  PCB#aikcp_pcb{rcv_buf = RcvBuf3, rcv_queue = RcvQ2, rcv_next = RcvNext2}.

data(Seg, -1, Prev,RcvBuf) -> 
  aikcp_buffer:insert(Prev, Seg, RcvBuf);
data(Seg, Idx, Prev, RcvBuf) ->
  Next = aikcp_buffer:next(Idx, RcvBuf),
  case aikcp_buffer:data(Idx, RcvBuf) of
    #aikcp_seg{sn = Sn} when Sn == Seg#aikcp_seg.sn -> RcvBuf;
    #aikcp_seg{sn = Sn} when ?DIFF_32(Seg#aikcp_seg.sn, Sn) > 0 ->
      data(Seg, Next, Idx, RcvBuf);
    #aikcp_seg{sn = Sn} when ?DIFF_32(Sn,Seg#aikcp_seg.sn) > 0 ->
      aikcp_buffer:insert(Prev, Seg, RcvBuf)
  end.

buffer_to_queue(RcvNext, RcvBuf, RcvQ) ->
  Idx = aikcp_buffer:head(RcvBuf),
  buffer_to_queue(RcvNext,Idx,-1, RcvBuf, RcvQ).
buffer_to_queue(RcvNext,-1,_, RcvBuf, RcvQ) ->
  {RcvNext, RcvBuf, RcvQ};
buffer_to_queue(RcvNext,Idx, Prev, RcvBuf, RcvQ) ->
  case aikcp_buffer:data(Idx, RcvBuf) of
    #aikcp_seg{sn = Sn} = Seg when Sn == RcvNext ->
      Next = aikcp_buffer:next(Idx, RcvBuf),
      RcvBuf2 = aikcp_buffer:delete(Idx, Prev, RcvBuf),
      RcvQ2 = aikcp_queue:push_back(Seg, RcvQ),
      Prev2 =
        case aikcp_buffer:head(RcvBuf2) == Next of
          true -> -1;
          false -> Prev
        end,
      buffer_to_queue(?BIT_32( RcvNext + 1),Next,Prev2,RcvBuf2,RcvQ2);
    _ -> {RcvNext, RcvBuf, RcvQ}
  end.
