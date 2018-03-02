-module(erlbq).
-behaviour(gen_server).

%% API exports
-export([
    %% api
    new/2,
    enqueue/2,
    dequeue/1,
    delete/1,

    %% behaviour
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).


-record(state, {
          capacity :: integer(),
          queue :: queue:queue()
         }).

-type state() :: #state{}.

%%====================================================================
%% API functions
%%====================================================================

-spec new(atom(), integer()) -> {ok, pid()} | {error, any()}.
new(Name, Capacity) ->
    gen_server:start_link({local, Name}, ?MODULE, [Capacity], []).

-spec enqueue(atom(), any()) -> ok.
enqueue(Name, Elem) ->
    gen_server:cast(Name, {enqueue, Elem}).

-spec dequeue(atom()) -> {ok, any()} | empty.
dequeue(Name) ->
    gen_server:call(Name, dequeue).

-spec delete(atom()) -> ok.
delete(Name) ->
    gen_server:stop(Name).

%%====================================================================
%% Server Callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init([Capacity]) ->
    {ok, #state{capacity=Capacity, queue=queue:new()}}.

-spec handle_cast({enqueue, any()}, state()) -> {noreply, state()}.
handle_cast({enqueue, Elem}, State=#state{capacity=Cap, queue=Queue}) ->
    case queue:len(Queue) < Cap of
        true ->
            {noreply, State#state{queue=queue:in(Elem, Queue)}};
        _ ->
            {noreply, State}
    end.

-spec handle_call(
        dequeue,
        any(),
        state()
       ) -> {reply, Reply, state()} 
                when Reply :: {value, any()} | empty.
handle_call(dequeue, _From, State=#state{queue=Queue}) ->
    {R, Q} = queue:out(Queue),
    {reply, R, State#state{queue=Q}}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Msg, State) ->
    %% Drop unexpected messages
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.
