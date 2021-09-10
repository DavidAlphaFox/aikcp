-module(aikcp_queue).

-export([new/0,
         size/1,
         pop_front/1,
         pop_back/1,
         push_front/2,
         push_back/2,
         front/1,
         back/1,
         empty/1]).

-record(aikcp_queue, {queue = queue:new(), size = 0}).

new() -> #aikcp_queue{}.

size(Queue) -> Queue#aikcp_queue.size.

pop_front(#aikcp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop(Q),
  Queue#aikcp_queue{queue = Q2, size = Size - 1}.

pop_back(#aikcp_queue{queue = Q, size = Size} = Queue) ->
  Q2 = queue:drop_r(Q),
  Queue#aikcp_queue{queue = Q2, size = Size - 1}.

push_back(Val,#aikcp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in(Val, Q),
  Queue#aikcp_queue{queue = Q2, size = Size + 1}.

push_front(Val,#aikcp_queue{queue = Q, size = Size}= Queue) ->
  Q2 = queue:in_r(Val, Q),
  Queue#aikcp_queue{queue = Q2, size = Size + 1}.

empty(#aikcp_queue{size = Size}) when Size =:= 0 -> true;
empty(_) -> false.

front(#aikcp_queue{queue = Q}) -> queue:get(Q).
back(#aikcp_queue{queue = Q})-> queue:get_r(Q).

