-module(aikcp_rtt).
-include("aikcp.hrl").
-export([handle/2]).

handle(RTT,PCB) when RTT < 0 -> PCB;
handle(RTT,PCB) ->
  PCB2 =
    if PCB#aikcp_pcb.rx_srtt == 0 ->
        PCB#aikcp_pcb{rx_srtt = RTT,
                      rx_rttval = RTT div 2};
       true ->
        Delta = abs(RTT - PCB#aikcp_pcb.rx_srtt),
        RxRttVal = (3 * PCB#aikcp_pcb.rx_rttval + Delta) div 4,
        RxSRttVal = (7 * PCB#aikcp_pcb.rx_srtt + RTT) div 8,
        RxSRttVal2 =
          if RxSRttVal < 1 -> 1;
             true -> RxSRttVal
          end,
        PCB#aikcp_pcb{rx_srtt = RxSRttVal2,
                      rx_rttval = RxRttVal}
    end,
  Rto = PCB2#aikcp_pcb.rx_srtt + ?MAX(PCB2#aikcp_pcb.interval, 4 * PCB2#aikcp_pcb.rx_rttval),
  Rto2 = aikcp_util:clamp(Rto,PCB2#aikcp_pcb.rx_minrto,?KCP_RTO_MAX),
  PCB2#aikcp_pcb{rx_rto = Rto2}.
