-module(nrgem_house_sup).

-behaviour(supervisor).

%% API
-export([%%start_link/0,
	     start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type), {Mod, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

%%start_link() ->
%%    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Id) ->
	lager:info("house_sup:start_link ~p",[Id]),
	ModId = list_to_atom(atom_to_list(?MODULE) ++ atom_to_list('$') ++ integer_to_list(Id)),
    supervisor:start_link({local, ModId }, ?MODULE, [Id]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Id) ->
    {ok, { {one_for_one, 5, 10}, [
        %%?CHILD(nrgem_houseroom_sup, Id, supervisor),
        { {Id}, {nrgem_house_disp, start_link, Id}, permanent, 5000, worker, [nrgem_house_disp]}]} }.