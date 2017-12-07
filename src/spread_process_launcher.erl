-module(spread_process_launcher).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PEERS_ROOT_PATH, <<"Process launcher">>).
-define(LOG_PATH, <<"latest_logs">>).
-define(COMMAND_PATH, <<"command">>).
-define(STATUS_PATH, <<"status">>).

%% JSON keys
-define(PID, <<"pid">>).
-define(STATUS, <<"status">>).
-define(STARTED, <<"started">>).
-define(TIME, <<"time">>).
-define(EXIT_STATUS, <<"exit_status">>).
-define(REF, <<"ref">>).

-define(DONE, <<"done">>).

-define(SELF, atom_to_binary(node(), utf8)).
-define(PROCESS_KEY_FROM_PORT(A), {A, process}).

-define(LOG_BUFFER_DURATION, 1000).

-record(process, {
    id,
    ref,
    port,
    line = <<>>,
    data = <<>>,
    data_timer,
    pid
    }).
-record(state, {
    me = ?SELF,
    processes = []
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @todo also check http://saleyn.github.io/erlexec/

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    FirstSet = spread:subscribe([?PEERS_ROOT_PATH, ?SELF], self()),
    lager:info("[spread_process_launcher] Launching with set ~p", [FirstSet]),
    [self() ! {update, Path, Iteration, Event} || {Path, Iteration, Event} <- FirstSet],
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update, [?PEERS_ROOT_PATH, Self, CommandId, ?COMMAND_PATH], _Timestamp, Event}, State) ->
    Command = spread_data:to_binary(spread_event:data(Event), self()),
    RefId = get_refid_from_event(Event),
    lager:info("New command for ~p: ~p", [CommandId, Command]),
    CurrentState = case spread:get([?PEERS_ROOT_PATH, Self, CommandId, ?STATUS_PATH]) of
        error ->
            no_process_yet;
        {_Iteration, _From, Status} ->
            case get_pid_from_status(Status) of
                undefined -> %% It was done
                    case get_refid_from_status(Status) of
                        RefId ->
                            do_not_launch;
                        _ ->
                            no_process_yet
                    end;
                PrevPid -> %% Was doing it, still not finished : kill it, since command might have changed
                    lager:info("Killing ~p", [PrevPid]),
                    kill_process(PrevPid),
                    no_process_yet
            end
    end,
    lager:info("Current state was ~p", [CurrentState]),
    case CurrentState of
        no_process_yet ->
            {Port, Pid} = start_process(Command),
            spread:post([?PEERS_ROOT_PATH, Self, CommandId, ?STATUS_PATH], jsx:encode([{?REF, RefId}, {?STATUS, ?STARTED}, {?PID, Pid}])),
            Process = #process{id = CommandId, ref = RefId, port = Port, pid = Pid},
            Key = ?PROCESS_KEY_FROM_PORT(Port),
            put(Key, Process);
        _ ->
            lager:info("Already done ~p, not launching", [Command]),
            not_launched
    end,
    {noreply, State};
handle_info({Port, {data, {MaybeEol, Data}}}, State) ->
    Key = ?PROCESS_KEY_FROM_PORT(Port),
    Process = get(Key),
    lager:info("Got ~p: ~p", [Process#process.id, Data]),
    NewProcess = case MaybeEol of
        eol ->
            TimerRef = case Process#process.data_timer of
                undefined ->
                    erlang:send_after(?LOG_BUFFER_DURATION, self(), {Port, post_lines});
                Ref ->
                    Ref
            end,
            Prefix = integer_to_binary(erlang:system_time(millisecond)),
            Process#process{
                data = <<(Process#process.data)/binary, Prefix/binary, " ", (Process#process.line)/binary, Data/binary, "\n">>,
                line = <<>>,
                data_timer = TimerRef
            };
        noeol ->
            Process#process{
                line = <<(Process#process.line)/binary, Data/binary>>
            }
    end,
    put(Key, NewProcess),
    {noreply, State};
handle_info({Port, post_lines}, State) ->
    Key = ?PROCESS_KEY_FROM_PORT(Port),
    case get(Key) of
        undefined -> %% Process died before
            ok;
        Process ->
            NewProcess = flush_log_lines(Process, State),
            put(Key, NewProcess)
    end,
    {noreply, State};
handle_info({Port, {exit_status, ExitStatus}}, State) ->
    Key = ?PROCESS_KEY_FROM_PORT(Port),
    Now = erlang:system_time(millisecond),
    Process = erase(Key),
    flush_log_lines(Process, State),
    ProcessId = Process#process.id,
    RefId = Process#process.ref,
    lager:info("Port ~p ended ~p", [Process, ExitStatus]),

    {_Iteration, _From, Status} = spread:get([?PEERS_ROOT_PATH, ?SELF, ProcessId, ?STATUS_PATH]),
    case get_refid_from_status(Status) of
        RefId ->
            spread:post([?PEERS_ROOT_PATH, ?SELF, ProcessId, ?STATUS_PATH], jsx:encode([{?REF, RefId}, {?STATUS, ?DONE}, {?TIME, Now}, {?EXIT_STATUS, ExitStatus}]));
        _ ->
            lager:info("Process has been replaced already, ignoring our kill exit")
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
start_process(Command) ->
    Port = open_port({spawn, Command}, [stream, binary, {line, 500}, exit_status]),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    {Port, to_gpid(OsPid)}.

kill_process(Pid) ->
    lager:info("Killing ~p", [Pid]),
    PidAsList = binary_to_list(Pid),
    os:cmd("while ps -p " ++ PidAsList ++ "; do kill -- -" ++ PidAsList ++ "; sleep 1; done;"),
    lager:info("Killed ~p", [Pid]).

to_gpid(Pid) ->
    [GPid | _] = re:split(os:cmd("ps -o pgid= " ++ integer_to_list(Pid) ++ " | grep -o [0-9]*"), "\n", [{return, binary}]),
    GPid.

flush_log_lines(Process, State) ->
    Self = State#state.me,
    CommandId = Process#process.id,
    spread:post([?PEERS_ROOT_PATH, Self, CommandId, ?LOG_PATH], Process#process.data),
    Process#process{data_timer = undefined, data = <<>>}.

get_pid_from_status(Status) ->
    try jsx:decode(Status) of
        JsxArgs ->
            proplists:get_value(?PID, JsxArgs)
    catch
        _:_ ->
            lager:error("Invalid JSON for status ~p", [Status]),
            undefined
    end.

get_refid_from_status(Status) ->
    try jsx:decode(Status) of
        JsxArgs ->
            proplists:get_value(?REF, JsxArgs)
    catch
        _:_ ->
            lager:error("Invalid JSON for status ~p", [Status]),
            undefined
    end.

get_refid_from_event(Event) ->
    spread_event:date(Event).

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
spread_process_launcher_test() ->
    application:ensure_all_started(?MODULE),
    spread:post([?PEERS_ROOT_PATH, ?SELF, <<"test1">>, ?COMMAND_PATH], <<"curl http://www.google.com 2>&1">>),
    timer:sleep(1500),
    Out = spread:subscribe([?PEERS_ROOT_PATH, ?SELF, <<"test1">>, ?LOG_PATH], self()),
    ?assertEqual(<<>>, Out).
-endif.
