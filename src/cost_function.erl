-module(cost_function).
-export([get_order_cost/7, get/3]).


-type button_dir() :: hall_up | hall_down | cab.
-record(o, {
    node :: term(),                    
    floor :: 0 .. 3,                    
    dir :: button_dir()                
    }).
-type order_rec() :: #o{}.
-type elevator_tuple() :: {term(), integer(), up | down | idle}.
-type order_tuple() :: {term(), integer(), hall_up| hall_down| cab}.

%% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%@doc Longer, more explicit version of cost_function:get/3.
%% ==Description== 
%% get_order_cost/7 works the same as get/3, but it forces the user to unpack everything, so that it might, hopefully become a bit easier to use
%%
%% Elevator_node and Order_node reffers to anything that can be used to separate two elevators capable of distinguishing two elevators from each other
%% ==Example==
%% 1> cost_function:get_order_cost( "My elevator", 1, up, "Other elevator", 3, hall_down, 4).
%% 
%% 4
%% 
%% 2>

-spec get_order_cost(
term(),
integer(),
up | down | idle,
atom()| pid(),
integer(),
hall_up| hall_down| cab,
integer()
)-> integer(). 
get_order_cost(
    Elevator_node,
    Elevator_floor,
    Elevator_direction,
    Order_node,
    Order_floor,
    Order_direction,
    Number_of_floors
)->
    Elevator = {Elevator_node,Elevator_floor,Elevator_direction},
    Order = {Order_node,Order_floor,Order_direction},
    get(Elevator, Order, Number_of_floors).

%%@doc Returns a cost of letting the elevator take an order.
%% ==Description==
%% Elevator is a tuple of an identifyer for the elevator, the floor it is on, and the state it is (up/down/idle)
%%
%% Order is a tuple of the identifyer for the elevator that made the order, the destination-floor and the direction it was made in (hall_up, hall_down, cab) 
%%
%% ==Example==
%%
%% 1> cost_function:get( {node(), 1, up}, {node(), 1, hall_down}, 4).
%% 
%% 5
%% 
%% 2>
-spec get(elevator_tuple(), order_tuple(), integer())-> integer(). 
get(Elevator, Order, Number_of_floors)-> cost_function(Elevator, Order, Number_of_floors).



%%@end
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cost_function(Elevator, Order, N_floors) ->
    true = is_sane(Elevator, Order,  N_floors),
    {E_node, E_floor, E_state} = Elevator, 
    {O_node, O_floor, O_dir} = Order,

    E_translated_dir =
        case E_state of
            up -> hall_up;
            down -> hall_down;
            idle -> idle
        end,
    Elev_rec  = #o{node = E_node, floor = E_floor, dir = E_translated_dir},
    Order_rec = #o{node = O_node, floor = O_floor, dir = O_dir},
    Start_cost =0,
    Cost = if 
        O_dir == cab andalso E_node =/= O_node -> infinity;
        E_state == idle -> abs(E_floor - O_floor);
        true -> simulator(Elev_rec, Order_rec, N_floors, Start_cost)
    end, 
    Cost.

-spec simulator(order_rec(),order_rec(), integer(), integer()) -> integer().

simulator( #o{floor=Floor, dir= Dir},#o{floor=Floor, dir=Dir},_,Cost)-> Cost;
simulator( #o{floor=Floor, dir=_Dir},#o{floor=Floor, dir=cab},_,Cost)-> Cost;
simulator(Elevator, Order, N_floors, Cost) when Cost < 9000->
    Floor = Elevator#o.floor, 
    New_elevator = if 
        Elevator#o.floor == N_floors -1, Elevator#o.dir == hall_up -> Elevator#o{dir=hall_down};   
        Elevator#o.floor == 0, Elevator#o.dir == hall_down -> Elevator#o{dir=hall_up};
        Elevator#o.dir == hall_up   -> Elevator#o{floor = Floor+1}; 
        Elevator#o.dir == hall_down -> Elevator#o{floor = Floor-1} 
    end,
    New_cost = case New_elevator#o.floor of 
        Floor -> Cost;
        _ -> Cost+1
    end,
    simulator(New_elevator, Order, N_floors, New_cost); 
simulator(Elev, Ord, N_floors, _) ->
    io:format('Simulator iterated too much, something there was a bug ~p~n', [{Elev, Ord, N_floors}]),
    exit("Simulator got infinite loop").



is_sane(Elev, Order, N_floors)->
    {_,E_floor, _} = Elev,
    {_,O_floor, O_dir} = Order,
    Is_sane = if
        E_floor >= N_floors                            -> false;
        E_floor < 0                                    -> false;
        O_floor >= N_floors                            -> false;
        O_floor < 0                                    -> false;
        O_dir == hall_down andalso O_floor =< 0        -> false;
        O_dir == hall_up andalso O_floor >= N_floors-1 -> false;
        true                                           -> true
    end,
    case Is_sane of 
        true -> Is_sane;
        false -> 
            io:format('Not sane:~p~n', [{Elev, Order, N_floors}]),
            Is_sane
    end.



% Debug
test_me() -> cost_function({node(), 0, up}, {node(), 2, hall_down}, 4).
