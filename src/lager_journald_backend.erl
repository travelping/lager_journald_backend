
%% @doc Journald backend for lager. Configured with a loglevel, formatter and formatter config.

-module(lager_journald_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level, formatter, formatter_config}).

%-include("lager.hrl").

-define(JOURNALD_FORMAT, [message]).
-define(CONTAINS(List,Elem), lists:any(fun(X)->X==Elem end, List)).
-define(LAGER_PROVIDED_META, [pid,file,line,module,function,node,date,time,message,severity]).
-define(ERL_PREFIXED_META, ?LAGER_PROVIDED_META ++ [application]).

%% @private
init(Config) ->
    [Level, Formatter, FormatterConfig] = [proplists:get_value(K, Config, Def) || {K, Def} <-
        [{level, info}, {formatter, lager_default_formatter}, {formatter_config, ?JOURNALD_FORMAT}]],
    State = #state{formatter=Formatter, formatter_config=FormatterConfig, level=lager_util:level_to_num(Level)},
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
handle_event({log, Message}, #state{level=L} = State) ->
    case lager_util:is_loggable(Message, L, ?MODULE) of
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

write(Msg, #state{formatter=F, formatter_config=FConf}) ->
    Text0 = F:format(Msg, FConf) -- ["\n"],
    Level = lager_msg:severity(Msg),
    Metadata = lager_msg:metadata(Msg),
    Metalist =  [{"MESSAGE", Text0},
                 {"PRIORITY", level_to_num(Level)}] ++
                [{journal_format(K), to_list(V)} || {K,V} <- Metadata,
                 not ?CONTAINS(?ERL_PREFIXED_META, K) ] ++
                [{"ERL_"++journal_format(K), to_list(V)} || {K,V} <- Metadata,
                 ?CONTAINS(?ERL_PREFIXED_META, K) ],
    ok = journald_api:sendv(Metalist).


% Adjustment of the key to the accepted formatting of Journald
journal_format(Key) ->
    HKey = string:to_upper(atom_to_list(Key)),
    re:replace(HKey, "[^a-zA-Z0-9]", "_", [global, {return, list}]).

level_to_num(debug) -> 7;
level_to_num(info) -> 6;
level_to_num(notice) -> 5;
level_to_num(warning) -> 4;
level_to_num(error) -> 3;
level_to_num(critical) -> 2;
level_to_num(alert) -> 1;
level_to_num(emergency) -> 0.

% convert all data types accordingly, so the journal is able to process them
% (see http://erlang.org/doc/reference_manual/data_types.html)
% ------------------------------------------------------------
% Number
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_float(V)   -> float_to_list(V);
% Atom
to_list(V) when is_atom(V)    -> atom_to_binary(V, utf8);
% Binary
% -- will be sent through without conversion
% Fun
to_list(V) when is_function(V)-> io_lib:format("~p", [V]);
% Port Identifier
to_list(V) when is_port(V)    -> io_lib:format("~p", [V]);
% Pid
to_list(V) when is_pid(V)     -> pid_to_list(V);
% Tuple
to_list(V) when is_tuple(V)   -> io_lib:format("~p", [V]);
% Map
to_list(V) when is_map(V)     -> io_lib:format("~p", [V]);
% List
to_list(V) when is_list(V)    -> list_to_string(V);
% String
% -- same as list
% Record
% -- same as tuple
% Boolean
% -- same as atoms
% anything else will given plainly to the journal api
to_list(V) -> V.

list_to_string (V) ->
  try io_lib:format('~s', [V])
  catch _:_ ->
    io_lib:format('~p', [V])
  end.
