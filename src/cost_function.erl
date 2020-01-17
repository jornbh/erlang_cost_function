-module(cost_function).

%% API exports

-export([explicit_cost_function/7, get/3]).

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
%%====================================================================
%% API functions
%%====================================================================

%% @doc Long function that makes you write all the arguments explixitly


explicit_cost_function(
    Elevator_node_id,
    Elevator_floor,
    Elevator_direction,
    Order_node_id,
    Order_flor,
    Order_direction,
    Number_of_floors
) when is_atom(Elevator_node_id)->
    ok.

%% @doc A shorter, more implicit cost function
-spec(get(#e{}, #o{}, integer())-> integer()).
get(
    {Elevator_node_id, Elevator_floor, Elevator_direction},
    {Order_node_id, Order_flor, Order_direction},
    Number_of_floors
) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================

motor_to_button_dir(Motor_dir)->
    M2B = #{up => hall_up, down=> hall_down, idle => cab},  
    map_get(Motor_dir, M2B). 

sanity_checker(Elevator =#e{}, Order =#o{}, N_floors)-> 
    Is_sane = if 
        Elevator#e.floor >= N_floors -> false; 
        Order#o.floor >= N_floors -> false; 
        true-> true
    end,
    case Is_sane of 
        true -> true; 
        _ -> 
            io:format('Bad input (~p, ~p,~p)~n', [Elevator, Order, N_floors]),
            false
    end.


%get_cost(elevator, order, n_floors)
get_cost(Elev, Order,N_floors)->
    true = sanity_checker(Elev, Order,N_floors),
    Cost = case {Elev, Order} of 
        {#e{node = Same}, #o{node=Same,dir=cab}}-> simulator(Elev, Order, N_floors); 
        {#e{}, #o{dir=cab}}-> infinity; 
        {#e{dir=idle},_} -> abs(Elev#e.floor - Order#o.floor);
        _-> simulator(Elev, Order, N_floors)
    end, 
    Cost.


simulator(_,_,_)-> todo. 

%TODO A lot more
% Basic debug tests for the module

test_me()-> 
    cost_function:explicit_cost_function(node(), 1, up, node(), 3, hall_up,  4),
    Elev = #e{node = node(), floor =1, dir = up},
    Ord =  #o{node = node(), floor =3, dir = hall_down},
    Cost1 = cost_function:get_cost(Elev, Ord, 4), 
    Cost2 = cost_function:get_cost(Elev#e{dir=idle}, Ord, 4), 
    io:format('Cost: ~p~n', [Cost1]),    
    io:format('Cost: ~p~n', [Cost2]),
    ok.    