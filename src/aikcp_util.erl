-module(aikcp_util).
-compile({inline,[wrapping_less/3]}).
-export([bit16/1,bit32/1,getaddr/1]).
-export([bit16_random/0,bit32_random/0]).
-export([microsecond/0,millisecond/0]).
-export([clamp/3,wrapping_less_bit32/2,wrapping_less_bit16/2]).

-spec bit16(integer()) -> integer().
bit16(N) when is_integer(N) -> N band 16#FFFF.

-spec bit32(integer()) -> integer().
bit32(N) when is_integer(N) -> N band 16#FFFFFFFF.

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
wrapping_less_bit32(L,R) -> wrapping_less(L,R,16#FFFFFFFF).
wrapping_less_bit16(L,R) -> wrapping_less(L,R,16#FFFF).

wrapping_less(L,R,Mask)->
  Down = (L - R) band Mask,
  Up = (R - L) band Mask,
  Up < Down.
