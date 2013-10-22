-module(nrgem_houseroom).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([add_consumer/1,
         tick/1]).


%% ------------------------------------------------------------------
%% Module callbacks
%% ------------------------------------------------------------------
-export([%%start_link/0, 
         start_link/1, 
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
       ]).

%% Publick API
add_consumer(ConsumerList) ->
    gen_server:call(?MODULE, {add_consumer, ConsumerList}).

tick(Hour) ->
    gen_server:call(?MODULE, {tick, Hour}).    
 
 
%% @private 
%%start_link() ->
	%%gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
start_link(Id) ->
    %%Name = atom_to_list(S) ++ "_" ++ atom_to_list(D),
    %%gen_server:start_link({local, list_to_atom(Name)}, ?MODULE, [S,D], []).
    gen_server:start_link(?MODULE, Id, []).
    

   
init(_Id) ->
    %%process_flag(trap_exit,true),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({add_consumer, ConsumerList}, _From, State) ->
    {reply,  {ok,ConsumerList}, State};

handle_call({tick, Hour}, _From, State) ->
    lager:info("~p say 'tick': ~p",[self(), Hour]),
    {reply,  {ok, Hour}, State};

handle_call(Message, _From, State)->
    lager:info("houseroom:handle_call ~p",[Message]),
    {reply, ok, State}.
    
handle_cast(Message, State) ->
    lager:info("houseroom:handle_cast ~p",[Message]),
    {noreply, State}.

%% @private
handle_info({tick, Hour}, State) ->
    lager:info("houseroom:handle_info {tick,~p}",[Hour]),
    {noreply, State};

handle_info(Message, State) ->
    lager:info("houseroom:handle_info ~p",[Message]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


consumer_ref(Name, ConsumerList) ->
    {Name, {nrgem_consumer, start_link, [ConsumerList]},
     permanent, 5000, worker, [nrgem_consumer]}.

%% INTERNAL
    
-ifdef(TEST).

simple_test() ->
    ok. %%= application:start(nrgem),
    %%?assertNot(undefined == whereis(nrgem_sup)).

-endif.
