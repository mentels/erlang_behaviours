-module(gen_server_callback).

-behaviour(gen_server).

%% API
-export([start_link/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {receiver :: pid()}).


%% API
start_link(TrapExit, Receiver) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [TrapExit, Receiver], []).

stop() ->
    gen_server:call(?SERVER, stop).


%% Callbacks
init([TrapExit, Receiver]) ->
    process_flag(trap_exit, TrapExit),
    {ok, #state{receiver = Receiver}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {stop, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, stop_normal}, State) ->
    {stop, normal, State};
handle_info({'EXIT', _Pid, stop_abnormal}, State) ->
    {stop, abnormal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{receiver = Receiver}) ->
    Receiver ! {self(), terminate_called, Reason}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
