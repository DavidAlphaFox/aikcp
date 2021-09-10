-module(aikcp_ctrl).
-compile({inline,[split/2,queue_send/2]}).

-include("aikcp.hrl").
-export([send/2,new/1]).


new(Conv)-> #aikcp_pcb{conv = Conv}.

send(<<>>,PCB)-> PCB;
send(Binary,#aikcp_pcb{stream = true,snd_queue = SndQ,mss = MSS} = PCB)->
    case aikcp_queue:empty(SndQ) of
      true -> queue_send(Binary,PCB);
      false ->
        Seg = aikcp_queue:back(SndQ),
        Diff = MSS - Seg#aikcp_seg.len,
        if
          Diff > 0 ->
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
  Frg2 = if
           Stream =:= true -> 0;
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
