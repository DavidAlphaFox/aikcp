-module(aikcp).
-include("aikcp.hrl").
-export([input/2,
         send/2,
         recv/1,
         new/1,
         new/2,
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
      aikcp_tx:handle(Binary, PCB)
  end.
new(Conv) ->
  aikcp_pcb:new(Conv).
update(PCB)->
  if PCB#aikcp_pcb.state < 0 ->
      {error,dead_link};
     true ->
      aikcp_pcb:handle(PCB)
  end.
new(Conv,Options)->
  PCB = new(Conv),
  setup(Options,PCB).

setup([],PCB) -> PCB;
setup([{stream, Stream}|T],PCB)->
  setup(T,PCB#aikcp_pcb{stream = Stream});
setup([{snd_wnd,SndWnd}|T],PCB) ->
  setup(T,PCB#aikcp_pcb{
            snd_wnd = SndWnd,
            cwnd = SndWnd,
            snd_buf = aikcp_buffer:new(SndWnd)});
setup([{rcv_wnd,RcvWnd}|T],PCB) ->
  setup(T,PCB#aikcp_pcb{
            rcv_wnd = RcvWnd,
            rcv_buf = aikcp_buffer:new(RcvWnd)});

setup([{nodelay,NoDelay}|T],PCB)->
  RxMinRto =
    if NoDelay == 0 -> ?KCP_RTO_MIN;
       true -> ?KCP_RTO_NDL
    end,
  setup(T,PCB#aikcp_pcb{nodelay = NoDelay,rx_minrto = RxMinRto});
setup([{interval,Interval}|T],PCB) ->
  Interval1 = aikcp_util:clamp(Interval, 10, 5000),
  setup(T,PCB#aikcp_pcb{interval = Interval1});
setup([{resend,Resend}|T],PCB) ->
  Resend1 = aikcp_util:clamp(Resend, 0, ?KCP_FASTACK_LIMIT - 1),
  setup(T,PCB#aikcp_pcb{fastresend = Resend1});
setup([{nocwnd,NC}|T],PCB) ->
  setup(T,PCB#aikcp_pcb{nocwnd = NC});
setup([{mtu, MTU}|T],PCB) ->
  MTU1 = aikcp_util:clamp(MTU,?KCP_OVERHEAD * 2,?KCP_MTU_DEF),
  setup(T,PCB#aikcp_pcb{mtu = MTU1,mss = MTU1 - ?KCP_OVERHEAD});
setup([_H|T],PCB) ->
  setup(T, PCB).

