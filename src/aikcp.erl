-module(aikcp).

-export([input/2,
         send/2,
         recv/1,
         new/1,
         update/1]).

input(Binary,PCB)->
  aikcp_rx:handle(Binary, PCB).
recv(PCB)->
  aikcp_rx:recv(PCB).
send(Binary,PCB)->
  aikcp_tx:send(Binary, PCB).
new(Conv) ->
  aikcp_pcb:new(Conv).
update(PCB)->
  aikcp_pcb:update(PCB).
