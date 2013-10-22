
-module(nrgem_houseroom_sup).

-behaviour(supervisor).

%% API
-export([
         add_houseroom/1,
         del_houseroom/2
        ]).

%% Supervisor callbacks
-export([
         %%start_link/0,
         start_link/1,
         init/1
        ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%start_link() ->
%%    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Id) ->
    supervisor:start_link({local, Id}, ?MODULE, [Id]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Id) ->
    Houseroom = {nrgem_houseroom, {nrgem_houseroom, start_link, [Id]},
                 temporary, 2000, worker, [nrgem_houseroom]},
    Children = [Houseroom],
    RestartStrategy = { simple_one_for_one, 0, 1},
    {ok, { RestartStrategy, Children }}.
    %%{ok, {{one_for_one, 3, 1}, [] }}.

% %% @private
houseroom_ref(Name) -> %%[].
    Id = list_to_atom(lists:flatten(io_lib:format("~p",[now()]))),
    [Name, Id].
    %%{ Name, { nrgem_houseroom, start_link, [Name]},
    %%        permanent, 2000, worker, [nrgem_houseroom]}.


add_houseroom(Name) ->
    Ref = houseroom_ref(Name),
    lager:info("houseroom ref: ~p",[Ref]),
    Pid = case supervisor:start_child(?MODULE, Ref) of
            {ok, Child} -> Child;
            {error, {already_started, Child}} -> Child;
            {error, Error} -> lager:error("error: ~p",[Error]);
            _-> lager:info("WTF")

        end,
    {ok, Pid}.

del_houseroom(Mod, Index) ->
    supervisor:terminate_child(?MODULE, {Mod, Index}),
    supervisor:delete_child(?MODULE, {Mod, Index}),
    ok.

% start_proxies(Mod) ->
%     lager:debug("Starting vnode proxies for: ~p", [Mod]),
%     Indices = get_indices(),
%     [start_proxy(Mod, Index) || Index <- Indices],
%     ok.



