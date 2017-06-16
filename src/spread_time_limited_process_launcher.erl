%%%-------------------------------------------------------------------
%% @doc spread_time_limited_process_launcher
%% @end
%%%-------------------------------------------------------------------

-module(spread_time_limited_process_launcher).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    self
}).

-define(PEERS_ROOT_PATH, <<"Time Limited Process Launcher">>).
-define(SELF, atom_to_binary(node(), utf8)).
-define(STOP_COMMAND, <<"echo time_is_up">>).

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Self = ?SELF,
    FirstSet = spread:subscribe([?PEERS_ROOT_PATH, Self], self()),
    [self() ! {update, Path, Iteration, Event} || {Path, Iteration, Event} <- FirstSet],
    erlang:send_after(60 * 1000, self(), {gc}),
    {ok, #state{
        self = Self
    }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update, [?PEERS_ROOT_PATH, Self, JobId], Timestamp, Event}, State) ->
    Args = spread_data:to_binary(spread_event:data(Event), self()),    
    try jsx:decode(Args) of
        JsxArgs ->
            Command = proplists:get_value(<<"command">>, JsxArgs),
            Validity = proplists:get_value(<<"validity">>, JsxArgs, 0),
            Now = erlang:system_time(second),
            Rest = Validity - Now,
            if
                Rest > 0 ->
                    case get({job, JobId}) of
                        undefined ->
                            spread:post([<<"Process launcher">>, Self, <<(?PEERS_ROOT_PATH)/binary, "_", JobId/binary>>, <<"command">>], Command);
                        {TimerRef, Command} ->
                            lager:debug("Setting new validity in ~p sec. to ~p", [Rest, Command]),
                            erlang:cancel_timer(TimerRef),
                    end,
                    NewTimerRef = erlang:send_after(Rest * 1000, self(), {check_validity, JobId, Timestamp}),
                    put({job, JobId}, {NewTimerRef, Command});
                true ->
                    stop_current_job(JobId, Self)
            end
    catch
        _:_ ->
            lager:error("Invalid JSON for channel ~p", [ChannelId])
    end,
    {noreply, State};
handle_info({check_validity, JobId, Timestamp}, State) ->
    Self = State#state.self,
    case spread:get([?PEERS_ROOT_PATH, Self, ChannelId]) of
        {Timestamp, _From, _Value} ->
            lager:info("Killing process since validity expired"),
            stop_current_job(JobId, Self);
        _ ->
            lager:info("Cancelling this check, since that restarted in the timeframe")
    end,
    {noreply, State};
handle_info({gc}, State) ->
    erlang:garbage_collect(self()),
    erlang:send_after(60 * 1000, self(), {gc}),
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
stop_current_job(JobId, Self) ->
    spread:post([<<"Process launcher">>, Self, <<(?PEERS_ROOT_PATH)/binary, "_", JobId/binary>>, <<"command">>], ?STOP_COMMAND).

