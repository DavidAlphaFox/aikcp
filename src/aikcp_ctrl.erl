-module(aikcp_ctrl).
-compile({inline,[split/2,queue_send/2]}).

-include("aikcp.hrl").
-export([send/2,new/1,update/2]).


new(Conv)-> #aikcp_pcb{conv = Conv}.

update(Now,#aikcp_pcb{updated = true} = PCB) -> do_update(Now,PCB);
update(Now,PCB) -> do_update(Now,PCB#aikcp_pcb{ts_flush = Now}).

do_update(Now,#aikcp_pcb{ts_flush = TsFlush,interval = Interval} = PCB)->
  Slap = Now - TsFlush,
  {Slap2, TsFlush2} =
    case (Slap >= 10000) or (Slap < -10000) of
      true -> {0, Now};
      false -> {Slap, TsFlush}
    end,
  case Slap2 >= 0 of
    false -> PCB#aikcp_pcb{current = Now, ts_flush = TsFlush2};
    true  ->
      TsFlush3 = TsFlush + Interval,
      TsFlush4 =
        if Now > TsFlush3 -> Now + Interval;
           true -> TsFlush3
        end,
      flush(PCB#aikcp_pcb{current = Now, ts_flush = TsFlush4})
  end.

flush(#aikcp_pcb{current = Now,ackcount = AckCount} = PCB)->
  {Buffer,PCB2} =
    if AckCount > 0 -> flush_ack(PCB);
       true -> {[],PCB}
    end,
  {Buffer,PCB2}.

flush_ack(#aikcp_pcb{conv = Conv,rcv_next = RcvNext} = PCB)->
  Wnd =  remain_wnd(PCB),
  flush_ack(Conv,Wnd,RcvNext,PCB).

flush_ack(Conv,Wnd,Una, #aikcp_pcb{acklist = AckList, mtu = MTU} = PCB) ->
  Buffer = flush_ack(Conv,Wnd,Una, AckList, [{[], 0}], MTU),
  {Buffer,PCB#aikcp_pcb{acklist = [],ackcount = 0}}.

flush_ack(_Conv,_Wnd,_Una, [], Buffer, Limit) -> build_buffer(undefined,Buffer,Limit);
flush_ack(Conv,Wnd,Una,[{Sn, Ts} | Left], Buffer, Limit) ->
  Bin = ?KCP_SEG(Conv, ?KCP_CMD_ACK, 0, Wnd, Ts, Sn, Una, 0, <<>>, <<>>),
  Buffer2 = build_buffer(Bin,Buffer,Limit),
  flush_ack(Conv,Wnd,Una,Left,Buffer2,Limit).


build_buffer(undefined,[{_,0}|T],_Limit) -> T;
build_buffer(undefined,[{BufferList,_}|T],_Limit) ->
  Buffer = join(BufferList),
  [Buffer|T];
build_buffer(undefined,[_H|_T] = Buffer,_Limit) -> Buffer;
build_buffer(Bin,[{BufferList,Size}|T],Limit) ->
  BinSize = byte_size(Bin),
  if (BinSize + Size + ?KCP_OVERHEAD) > Limit ->
      Buffer = join(BufferList),
      [{[Bin],BinSize},Buffer|T];
     true -> [{[Bin|BufferList],Size + BinSize}|T]
  end.



remain_wnd(#aikcp_pcb{rcv_wnd = RcvWnd, rcv_queue = RcvQ})->
  Diff = RcvWnd - aikcp_queue:size(RcvQ),
  if Diff > 0 -> Diff;
     true -> 0
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
