-module(nrgem_powertrans_disp).
-behaviour(gen_server).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
          add_house/1,
          tick/1,
          test/1
        ]).


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


%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type), {Mod, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% Publik API
add_house(Id) ->
    gen_server:call(?MODULE, {add_house, Id}). 

tick(Hour) ->
    gen_server:call(?MODULE, {tick,Hour}).

test(Cnt) ->
    gen_server:call(?MODULE, {test, Cnt}). 
 
%% @private 
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
   
init([]) ->
    {ok, []}.
    
   

    
%% @private  
handle_call({tick,Hour}, _From, State) ->
    Childs = supervisor:which_children(nrgem_powertrans_sup),
    [ Pid ! {tick,Hour} || {_,Pid,worker,[_]} <- Childs],
    {reply, ok, State};

handle_call({test,Cnt}, _From, State) ->
    %% nrgem_houseroom_sup:add_houseroom(hr1),
    %% nrgem_houseroom_sup:add_houseroom(hr2),
    %% nrgem_houseroom_sup:add_houseroom(hr3),
    %% nrgem_houseroom_sup:add_houseroom(hr4),
    %% nrgem_houseroom_sup:add_houseroom(hr5),
    {reply, ok, State};    

handle_call({add_house, Id}, _From, State) ->
    ChildRef = ?CHILD(nrgem_house_sup, [Id], supervisor),
    Pid = supervisor:start_child(nrgem_powertrans_sup, ChildRef),
    {reply,  {ok, Pid}, State};

handle_call(Msg, _From, State) ->
    lager:info("nrgem_powertrans_disp:handle_call",[Msg]),
    {reply,  ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
-ifdef(TEST).

simple_test() ->
    ok. %%= application:start(nrgem),
    %%?assertNot(undefined == whereis(nrgem_sup)).

-endif.
