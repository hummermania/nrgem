-module(nrgem_default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Headers = [{<<"Location">>, "/index.html"}],
    {ok, Req2} = cowboy_req:reply(301, Headers, <<>>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

