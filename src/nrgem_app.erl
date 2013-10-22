%% Copyright (c) 2013+, Markevych Alexander <rabota.pmr@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(nrgem_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2]).
-export([stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
%%	application:start(crypto),
%%	application:start(public_key),
%%	application:start(ssl),
    lager:info("nrgem_app:start()"),
    
    application:start(lager),    
    application:start(crypto),
    application:start(mimetypes),
    application:start(ranch),
    application:start(cowboy),
    application:start(jsx),
    application:start(bullet),
    application:start(nrgem),
    appmon:start().

start(_Type, _tArgs) ->
    PrivDir = code:priv_dir(nrgem),
    BuildDir =    abs_path(filename:join([PrivDir, "nrgem"])),
    JQueryDir =   abs_path(filename:join([PrivDir, "jquery"])),
    JQueryUiDir = abs_path(filename:join([PrivDir, "jquery-ui"])),            
    FlotDir =     abs_path(filename:join([PrivDir, "flot"])),

    BulletDir = abs_path(code:priv_dir(bullet)),
    StaticFilesCfg = [{mimetypes, {fun mimetypes:path_to_mimes/2, default}}],

    Dispatch = cowboy_router:compile([{'_', [
	    {"/stream", bullet_handler, [{handler, nrgem_stream_handler}]},

	    {"/", nrgem_default_handler, []},

            {"/jquery/[...]", cowboy_static,
                [{directory, JQueryDir}|StaticFilesCfg]},
                 
            {"/jquery-ui/[...]", cowboy_static,
                [{directory, JQueryUiDir}|StaticFilesCfg]},

            {"/flot/[...]", cowboy_static,
                [{directory, FlotDir}|StaticFilesCfg]},

            {"/bullet/[...]", cowboy_static,
                [{directory, BulletDir}|StaticFilesCfg]},

            {"/[...]", cowboy_static,
                [{directory, BuildDir}|StaticFilesCfg]}
		]}
    ]),
    {ok, _} = cowboy:start_http(nrgem_http, 100, [{port, 1080}], 
                                [ {env, [{dispatch, Dispatch}]} ]),
	nrgem_sup:start_link().

stop(_State) ->
	ok.


%%
%% Private
%%

abs_path(Path) -> 
    filename:join(
        abs_path_(
            filename:split(
                filename:absname(Path)), [])).

abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).

-ifdef(TEST).

simple_test() ->
    ok = application:start(nrgem),
    ?assertNot(undefined == whereis(nrgem_sup)).

-endif.
