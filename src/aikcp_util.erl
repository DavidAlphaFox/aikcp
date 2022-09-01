-module(aikcp_util).
-compile({inline,[clamp/3,join/1,split/2]}).
-export([getaddr/1]).
-export([clamp/3]).
-export([bit16_random/0,bit32_random/0]).
-export([microsecond/0,millisecond/0]).
-export([join/1,split/2]).

getaddr(S) when is_list(S) ->
  {ok, CAddr} = inet:getaddr(S, inet),
  CAddr;
getaddr({_, _, _, _} = Addr) -> Addr.

microsecond()-> os:system_time(microsecond).
millisecond()-> os:system_time(millisecond).


bit16_random() ->
  <<N:16/integer>> = crypto:strong_rand_bytes(2),
  N.

bit32_random()->
  <<N:32/integer>> = crypto:strong_rand_bytes(4),
  N.

clamp(Val, Min, _Max) when Val < Min -> Min;
clamp(Val, _Min, Max) when Val > Max -> Max;
clamp(Val, _Min, _Max) -> Val.

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
  lists:foldr(
    fun (I, Acc) ->  <<Acc/binary,I/binary>> end,
    <<>>, List).
