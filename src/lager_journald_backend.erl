%% @doc Journald backend for lager. Configured with a loglevel, formatter and formatter config.

-module(lager_journald_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level, formatter, formatter_config, service, level_overrides}).

%-include("lager.hrl").

-define(JOURNALD_FORMAT, [message]).

%% @private
init(Config) ->
    [Level, Formatter, FormatterConfig, LevelOverrideList] = [proplists:get_value(K, Config, Def) || {K, Def} <- 
        [{level, info}, {formatter, lager_default_formatter}, {formatter_config, ?JOURNALD_FORMAT}, {level_overrides, []}]],
    Service = application:get_env(lager, service, "unknown"),
    %% level_overrides is a list of {module, level}, i.e. [{foo, info}, {bar, warning}]
    LevelOverrides = maps:from_list(lists:map(fun ({M, L}) -> {M, lager_util:level_to_num(L)} end, LevelOverrideList)),
    State = #state{formatter=Formatter, formatter_config=FormatterConfig, level=lager_util:level_to_num(Level), service=Service, level_overrides=LevelOverrides},
    {ok, State}.

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    try lager_util:level_to_num(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message}, #state{} = State) ->
    case lager_util:is_loggable(Message, get_effective_level(Message, State), ?MODULE) of
        true ->
            ok = write(Message, State),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

write(Msg, #state{formatter=F, formatter_config=FConf, service=Service}) ->
    Text0 = F:format(Msg, FConf) -- ["\n"],
    Level = lager_msg:severity(Msg),
    Metadata = lager_msg:metadata(Msg),
    ok = journald_api:sendv(
        [{"SERVICE", Service},
         {"MESSAGE", Text0}, 
         {"SYSLOG_IDENTIFIER", "container"}, 
         {"SYSLOG_FACILITY", 3}, 
         {"PRIORITY", level_to_num(Level)}] ++
        lists:flatmap(fun format_metadata/1, Metadata)
    ).

% don't log file and line
format_metadata({file, _}) -> [];
format_metadata({line, _}) -> [];
% only log defined built-in tags
format_metadata({application, undefined}) -> [];
format_metadata({application, App}) -> [{"APPLICATION", App}];
format_metadata({pid, undefined}) -> [];
format_metadata({pid, Pid}) -> [{"PID", Pid}];
format_metadata({node, undefined}) -> [];
format_metadata({node, Node}) -> [{"ERLANG_NODE", Node}];
format_metadata({function, undefined}) -> [];
format_metadata({function, CodeFunc}) -> [{"CODE_FUNC", CodeFunc}];
format_metadata({module, undefined}) -> [];
format_metadata({module, CodeFile}) -> [{"CODE_FILE", CodeFile}];
% log all custom tags
format_metadata({CustomTag, Value}) when is_binary(Value) ->
    case uuid:is_uuid(Value) of
        true -> [{atom_to_list(CustomTag), uuid:uuid_to_string(Value)}];
        false -> [{atom_to_list(CustomTag), Value}]
    end;
format_metadata({CustomTag, Value}) -> [{atom_to_list(CustomTag), Value}].


level_to_num(debug) -> 7;
level_to_num(info) -> 6;
level_to_num(notice) -> 5;
level_to_num(warning) -> 4;
level_to_num(error) -> 3;
level_to_num(critical) -> 2;
level_to_num(alert) -> 1;
level_to_num(emergency) -> 0.

get_effective_level(Message, #state{level = L, level_overrides = LevelOverrides}) ->
    case lists:keyfind(module, 1, lager_msg:metadata(Message)) of
        {module, Module} ->
            case maps:find(Module, LevelOverrides) of
                {ok, Override} -> Override;
                _ -> L
            end;
        _ -> L
    end.
