-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test1/1, test2/1]).


-record(e,{
    node, 
    floor :: integer(), 
    dir :: up | down | idle
}).
-record(o,{
    node, 
    floor, 
    dir
}).
all() -> [test1, test2].

test1(_Config) -> 1 = 1.

test2(_Config) ->
    cost_function:explicit_cost_function(node(), 1, up, node(), 3, hall_up,  4),
    Elev = #e{node = node(), floor =1, dir = up},
    Ord =  #o{node = node(), floor =3, dir = hall_down},
    Cost1 = cost_function:get_cost(Elev, Ord, 4), 
    Cost2 = cost_function:get_cost(Elev#e{dir=idle}, Ord, 4), 
    io:format('Cost: ~p~n', [Cost1]),    
    io:format('Cost: ~p~n', [Cost2]),    
    A = 1,
    1 / A.

