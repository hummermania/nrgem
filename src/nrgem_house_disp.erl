-module(nrgem_house_disp).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
          add_houseroom/1,
          tick/1,
          test/1
        ]).


%% ------------------------------------------------------------------
%% Module callbacks
%% ------------------------------------------------------------------
-export([start_link/1, 
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3
       ]).


%% Publik API
add_houseroom(Name) ->
    gen_server:call(?MODULE, {add_houseroom, Name}). 

tick(Hour) ->
    gen_server:call(?MODULE, {tick,Hour}).

test(Cnt) ->
    gen_server:call(?MODULE, {test, Cnt}). 
 
%% @private 
start_link(Id) ->
    ModId = list_to_atom(atom_to_list(?MODULE) ++ atom_to_list('$') ++ integer_to_list(Id)),
    gen_server:start_link({local, ModId}, ?MODULE, [], [Id]).
    
   
init([]) ->
    {ok, []}.
    
   

    
%% @private  
handle_call({tick,Hour}, _From, State) ->
    Childs = supervisor:which_children(nrgem_houseroom_sup),
    %% Send tick signal for all houserooms.
    [ Pid ! {tick,Hour} || {_,Pid,worker,[_]} <- Childs],
    {reply, ok, State};

handle_call({test,Cnt}, _From, State) ->
    nrgem_houseroom_sup:add_houseroom(hr1),
    nrgem_houseroom_sup:add_houseroom(hr2),
    nrgem_houseroom_sup:add_houseroom(hr3),
    nrgem_houseroom_sup:add_houseroom(hr4),
    nrgem_houseroom_sup:add_houseroom(hr5),
    {reply, ok, State};    

handle_call({add_houseroom, Name}, _From, State) ->
    Pid = nrgem_houseroom_sup:add_houseroom(Name),
    {reply,  {ok, Pid}, State};

handle_call(_Msg, _From, State) ->
    lager:info("nrgem_house_disp:handle_call"),
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
