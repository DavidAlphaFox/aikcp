-moduule(aikcp_queue).

-export([new/0,
         len/1,
         drop/1,
         get/1,
         in/2,
         is_empty/1]).

-record(aikcp_queue, {queue = queue:new(), len = 0}).

new() -> #aikcp_queue{}.

len(Queue) -> Queue#aikcp_queue.len.

drop(Queue) ->
  #aikcp_queue{queue = Q, len = Len} = Queue,
  Q2 = queue:drop(Q),
  Queue#aikcp_queue{queue = Q2, len = Len - 1}.

is_empty(#aikcp_queue{len = Len}) when Len =:= 0 -> true;
is_empty(_) -> false.

get(#aikcp_queue{queue = Q}) -> queue:get(Q).

in(Val,#aikcp_queue{queue = Q, len = Len}= Queue) ->
  Q2 = queue:in(Val, Q),
  Queue#aikcp_queue{queue = Q2, len = Len + 1}.
