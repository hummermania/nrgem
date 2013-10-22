-module(nrgem_consumer_type).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([schedule/1]).

schedule([]) -> [];

schedule([H|T] = _ConsumerTypes) ->
    schedule(list_to_atom(binary_to_list(H))) ++ schedule(T);

schedule(lamp_wolfram) ->
    Power = 100,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
     [{6,0},  {6,45}, Power], [{7,0}, {7,40},Power],
     [{8,15}, {9,25},Power],  [{11,0},{13,30},Power]];

schedule(lamp_save10) ->
    Power = 10,
    [[{6,30}, {7,40},  Power], [{8,15}, {9,25},  Power], 
     [{11,0},{13,30}, Power],  [{17,0}, {17,25},Power], 
     [{18,35},{20,50},Power]];

schedule(lamp_save18) ->
    Power = 18,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
    [{6,0},  {6,45}, Power],  [{7,0}, {7,40},Power],
    [{8,15}, {9,25},Power],   [{11,0},{13,30},Power]];

schedule(lamp_save25) ->
    Power = 25,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
    [{6,0},  {6,45}, Power],  [{7,0}, {7,40},Power],
    [{8,15}, {9,25},Power],   [{11,0},{13,30},Power]];    

schedule(tv_small) ->
    Power = 30,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
    [{6,0},  {6,45}, Power],  [{7,0}, {7,40},Power],
    [{8,15}, {9,25},Power],   [{11,0},{13,30},Power]];    

schedule(tv_middle) ->
    Power = 55,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
    [{6,0},  {6,45}, Power],  [{7,0}, {7,40},Power],
    [{8,15}, {9,25},Power],   [{11,0},{13,30},Power]];    

schedule(tv_led) ->
    Power = 48,
    [[{7,0},  {11,25},Power], [{12,35}, {14,50},Power],
    [{15,20}, {16,45},Power], [{17,50}, {19,40},Power],
    [{20,15}, {22,35},Power]];

schedule(tv_plasma) ->
    Power = 18,
    [[{6,45}, {7,25},Power], [{18,15},{19,20},Power],
    [{20,50}, {22,45}, Power]];    

schedule(pc_old) ->
    Power = 280,
    [[{7,0},  {12,50}, Power],  [{13,05}, {17,40},Power],
    [{20,15}, {22,5},Power]];

schedule(pc_p4) ->
    Power = 250,
    [[{17,0}, {17,25},Power], [{18,35},{20,50},Power],
    [{6,0},  {6,45}, Power],  [{7,0}, {7,40},Power],
    [{8,15}, {9,25},Power],   [{11,0},{13,30},Power]];    

schedule(pc_c2duo) ->
    Power = 340,
    [[{7,40},  {18,20}, Power]];    

schedule(pc_c4game) ->
    Power = 430,
    [[{13,20}, {17,25},Power], [{20,00},{23,10},Power]];

schedule(_) ->
    lager:info("Bad argument of nrgem_consumer_type:schedule()").
    
-ifdef(TEST).

simple_test() ->
    ok. %%= application:start(energy),
    %%?assertNot(undefined == whereis(energy_sup)).
-endif.