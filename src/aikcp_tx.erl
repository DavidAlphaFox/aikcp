-module(aikcp_tx).
-compile({inline,[send/2]}).
-include("aikcp.hrl").
-export([handle/2]).

handle(<<>>,PCB)-> PCB;
handle(Binary,#aikcp_pcb{mss = MSS})
  when byte_size(Binary) >= MSS * 128->
  {error,fragment_overflow};
handle(Binary,#aikcp_pcb{stream = true,snd_queue = SndQ,mss = MSS} = PCB)->
  case aikcp_queue:empty(SndQ) of
    true -> send(Binary,PCB);
    false ->
      Seg = aikcp_queue:back(SndQ),
      Diff = MSS - Seg#aikcp_seg.len,
      if Diff > 0 ->
          <<Fill:Diff/bytes,Left/binary>> = Binary,
          Data = Seg#aikcp_seg.data,
          Data2 = <<Data/binary,Fill/binary>>,
          Seg2 = Seg#aikcp_seg{data = Data2, len = byte_size(Data2)},
          SndQ2 = aikcp_queue:push_back(Seg2,aikcp_queue:pop_back(SndQ)),
          send(Left,PCB#aikcp_pcb{snd_queue = SndQ2});
         true -> send(Binary,PCB)
      end
  end;
handle(Binary,PCB) -> send(Binary,PCB).

send(Binary,#aikcp_pcb{mss = MSS} = PCB)->
  Payloads = aikcp_util:split(Binary,MSS),
  send(Payloads,length(Payloads),PCB).

send([], _,PCB) -> PCB;
send( [Data | Left], Frg,#aikcp_pcb{stream = Stream,snd_queue = SndQue} = PCB) ->
  %% 流模式下无分片
  Frg2 =
    if Stream == true -> 0;
       true -> Frg -1
    end,
  Seg = #aikcp_seg{len = byte_size(Data), frg = Frg2, data = Data},
  send(Left, Frg2,PCB#aikcp_pcb{snd_queue = aikcp_queue:push_back(Seg, SndQue)}).

