-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-import(cost_core, [cost_function/5]).
-import(list, [seq/2]).

-export([all/0]).
-export([test1/1, test2/1]).

% -record(e,{
%     node,
%     floor :: integer(),
%     dir :: up | down | idle
% }).
% -record(o,{
%     node,
%     floor,
%     dir
% }).
all() -> [test1, test2].

test1(_Config) -> 1 = 1.

%% @doc Positive tests
test2(_Config) ->
    N_floors = 4,
    Valid_results =
        [
            cost_function:get({E_node, E_floor, E_dir}, {O_node, O_floor, O_dir}, N_floors)
            || E_node <- [node(), other_node],
            E_dir <- [up, down, idle],
            E_floor <- lists:seq(0, 3),
            O_node <- [node(), other_node],
            O_dir <- [hall_up, hall_down, cab],
            O_floor <- lists:seq(0, 3),
            not (O_dir == hall_up andalso O_floor == N_floors -1),
            not (O_dir == hall_down andalso O_floor == 0)

        ],
    io:format('Results ~p~n', [Valid_results]).
