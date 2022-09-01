-module(aikcp).
-include("aikcp.hrl").
-export([input/2,
         send/2,
         recv/1,
         new/1,
         update/1]).

input(Binary,PCB)->
  if PCB#aikcp_pcb.state < 0 ->
      {error,dead_link};
     true ->
      aikcp_rx:handle(Binary, PCB)
  end.
recv(PCB)->
  if PCB#aikcp_pcb.state < 0 ->
      {error,dead_link};
     true ->
      aikcp_rx:recv(PCB)
  end.
send(Binary,PCB)->
  if PCB#aikcp_pcb.state < 0 ->
      {error,dead_link};
     true ->
      aikcp_tx:send(Binary, PCB)
  end.
new(Conv) ->
  aikcp_pcb:new(Conv).
update(PCB)->
  if PCB#aikcp_pcb.state < 0 ->
      {error,dead_link};
     true ->
      aikcp_pcb:update(PCB)
  end.

