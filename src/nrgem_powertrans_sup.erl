-module(nrgem_powertrans_sup).

-behaviour(supervisor).

-export([add_house/1]).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type), {Mod, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, 
           [%%?CHILD(nrgem_house_sup,       [], supervisor),
            ?CHILD(nrgem_powertrans_disp, [], worker)]} }.


% %% @private
house_ref(Name) -> %%[].
    %%Id = list_to_atom(lists:flatten(io_lib:format("~p",[now()]))),
    Id = erlang:phash2(Name),
    %%?CHILD(nrgem_house_sup, Id, supervisor).
    { {Name,Id}, { nrgem_house_sup, start_link, [Id]},
                         permanent, 2000, supervisor, [nrgem_house_sup]}.


add_house(Name) ->
    Ref = house_ref(Name),
    lager:info("new house ref: ~p",[Ref]),
    Pid = case supervisor:start_child(?MODULE, Ref) of
            {ok, Child} -> Child;
            {error, {already_started, Child}} -> 
                lager:error("Child already started: ~p",[Child]), 
                Child;
	        {error, Error} -> lager:error("error: ~p",[Error]);
            _-> lager:info("WTF")

        end,
    {ok, Pid}.
