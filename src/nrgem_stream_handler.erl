-module(nrgem_stream_handler).
-export([init/4, stream/3, info/3, terminate/2]).
-export([send/2]).

-record(state, {
}).

-define(CONSUMER, nrgem_consumer).

init(_Transport, Req, _Opts, _Active) ->
    State = #state{
    },

    {ok, Req, State}.

stream(<<"ping">> = _Data, Req, State) ->
    lager:info("<<ping>> signal detected"),
    Data = [1,2,3,4],
    Respond = [{'event', <<"dataLoadCompleted">>} 
              ,{'data', [{'rows', Data}]}
              ],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

stream(Data, Req, State) ->
    DecodedData = jsx:json_to_term(Data),
    %%lager:info("Request: ~p", [Req]),
    case proplists:get_value(<<"nrg_event">>, DecodedData) of
        <<"tick">> -> 

            Hour = proplists:get_value(<<"hour">>, DecodedData),
            ConsumerType = proplists:get_value(<<"consumer_type">>,DecodedData),
            %%lager:info("Tick. hour:~p, cons_type:~p", [Hour, ConsumerType]),
        
            {ok,Power} = ?CONSUMER:tick(ConsumerType,
                list_to_integer(binary_to_list(Hour))),

            case Hour of
                <<"0">> -> 
                    lager:info("Hour = ~p, Power = ~p",[Hour,Power]);
                _ -> ok
            end,
        
            Respond = [{'nrg_event', <<"power">>} 
                      ,{'power',list_to_binary(number_to_list(Power))}
                      ,{'hour',Hour}],

            EncodedRespond = jsx:term_to_json(Respond),
                {reply, EncodedRespond, Req, State};

        <<"sched_set">> ->
            Sched_start = proplists:get_value(<<"sched_start">>, DecodedData),
            Sched_end =   proplists:get_value(<<"sched_end">>, DecodedData),
            Sched_pwr =   proplists:get_value(<<"sched_pwr">>, DecodedData),
            lager:info("Start = ~p, End = ~p, Power = ~p",
                [Sched_start, Sched_end, Sched_pwr]),
            Respond = [{'event', <<"sched_set">>} 
                        ,{'data', <<"ok">>}],
            EncodedRespond = jsx:term_to_json(Respond),
            {reply, EncodedRespond, Req, State};        

        _ -> 
            lager:info("Some request: Data = ~p, Req = ~p",[Data,Req]),
            {reply, Data, Req, State}
    end.




info({'diff_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataUpdated">>} 
              ,{'data', [{'rows', Rows}]}],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'add_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataAdded">>} 
              ,{'data', [{'rows', Rows}]}],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'delete_list', Rows}=_Info, Req, State) ->
    Respond = [{'event', <<"dataRemoved">>} 
              ,{'data', [{'rows', Rows}]}],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State};

info({'log_event', Mess}=_Info, Req, State) ->
    Respond = [{'event', <<"logEvent">>} 
              ,{'data', Mess}],
    EncodedRespond = jsx:term_to_json(Respond),
    {reply, EncodedRespond, Req, State}.



terminate(_Req, _State) ->
    ok.


%%
%% API
%%

send(Pid, Mess) ->
    Pid ! Mess.

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

number_to_list(L) ->
    try float_to_list(L)
    catch
        error:badarg ->
            integer_to_list(L)
    end.

%%binary_to_existing_atom(X) ->
%%    list_to_existing_atom(binary_to_list(X)).


%%atom_to_binary(X) ->
%%    list_to_binary(atom_to_list(X)).
