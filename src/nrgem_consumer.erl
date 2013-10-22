-module(nrgem_consumer).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([tick/2]).


%% ------------------------------------------------------------------
%% Module callbacks
%% ------------------------------------------------------------------
-export([start_link/0, 
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
       ]).


%% Publik API
tick(ConsumerType,Hour) ->
    gen_server:call(?MODULE, [{consumer_type, ConsumerType}, {tick, Hour}]). 
 
 
%% @private 
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
   
init([]) ->
    {ok, []}.
    
   

    

%% @private  
handle_call([{consumer_type, ConsumerType}, {tick,Hour}], _StateName, State) ->
    %% Get schedule consumer work  from list of  consumer types
    Schedule = nrgem_consumer_type:schedule(ConsumerType),
    %%lager:info("Consumer schedule: ~p",[Schedule]),
    Power = power(Schedule, Hour),
    %%lager:info("Schedule:~p, Power:~p, Hour:~p",[Schedule,Power,Hour]),
    {reply,  {ok,Power}, State}.
    
%% @private
handle_cast(_, State) ->
    {noreply, State}.

%% @private
%%handle_info(broadcast, State) ->
%%    S2 = broadcast(State#state.peers, State),
%%    {noreply, S2}.


handle_info(_, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% INTERNAL
power([],_) -> 0;

power([H|T], Hour)->
    power_calc(H,Hour) + power(T,Hour).

    %%lager:info("Power:~p, ~p, Hour:~p",[Power1,H,Hour])


power_calc([{BegHour, BegMinute},{EndHour,_EndMinute}, Pwr],Hour) 
    when BegHour =:= Hour-1, EndHour >= Hour -> 
        (60 - BegMinute)/60*Pwr;

power_calc([{BegHour, BegMinute},{EndHour, EndMinute}, Pwr],Hour) 
    when BegHour =:= Hour-1, EndHour =:= Hour-1 -> 
        (EndMinute - BegMinute)/60*Pwr;

power_calc([{BegHour,_BegMinute},{EndHour, EndMinute}, Pwr],Hour) 
    when BegHour < Hour-1, EndHour =:= Hour-1 -> 
        EndMinute/60*Pwr;

power_calc([{BegHour,_BegMinute},{EndHour,_EndMinute}, Pwr],Hour) 
    when BegHour < Hour-1, EndHour >= Hour -> Pwr;

power_calc([{BegHour,_BegMinute},{EndHour,_EndMinute},_Pwr],Hour) 
    when BegHour < Hour-1, EndHour < Hour-1  -> 0;

power_calc([{BegHour,_BegMinute},{EndHour,_EndMinute},_Pwr],Hour) 
    when BegHour >= Hour ,  EndHour >= Hour -> 0.






% broadcast(Nodes, State) ->
%     case (State#state.status) of
%         up ->
%             Msg = {up, node(), State#state.services};
%         down ->
%             Msg = {down, node()}
%     end,
%     {Mod, Fn} = State#state.bcast_mod,
%     Mod:Fn(Nodes, ?MODULE, Msg),
%     schedule_broadcast(State).

% schedule_broadcast(State) ->
%     case (State#state.bcast_tref) of
%         undefined ->
%             ok;
%         OldTref ->
%             erlang:cancel_timer(OldTref)
%     end,
%     Interval = app_helper:get_env(riak_core, gossip_interval),
%     Tref = erlang:send_after(Interval, self(), broadcast),
%     State#state { bcast_tref = Tref }.
    
%%select(X, L) ->  
%%    [Y || {X1, Y} <- L, X == X1].    
    
-ifdef(TEST).

simple_test() ->
    ok. %%= application:start(nrgem),
    %%?assertNot(undefined == whereis(nrgem_sup)).

-endif.
