-module(erlbq).
-behaviour(gen_server).

%% API exports
-export([
    %% api
    new/2,
    enqueue/2,
    enqueue_cast/2,
    dequeue/1,
    subscribe/2,
    unsubscribe/2,
    delete/1,

    %% behaviour
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-opaque handle() :: reference().

-export_type([
    handle/0
  ]).

-type func() :: {module(), atom(), list()}.
-record(state, {
          capacity = 0 :: integer(),
          queue = queue:new() :: queue:queue(),
          subscribers = #{} :: #{handle() => func()}
         }).

-type state() :: #state{}.

%%====================================================================
%% API functions
%%====================================================================

-spec new(atom(), integer()) -> {ok, pid()} | {error, any()}.
new(Name, Capacity) ->
    gen_server:start_link({local, Name}, ?MODULE, [Capacity], []).

-spec enqueue(atom(), any()) -> ok | full.
enqueue(Name, Elem) ->
    gen_server:call(Name, {enqueue, Elem}).

-spec enqueue_cast(atom(), any()) -> ok.
enqueue_cast(Name, Elem) ->
    gen_server:cast(Name, {enqueue, Elem}).

-spec dequeue(atom()) -> {ok, any()} | empty.
dequeue(Name) ->
    gen_server:call(Name, dequeue).

-spec subscribe(atom(), func()) -> handle().
subscribe(Name, Func) ->
    gen_server:call(Name, {subscribe, Func}).

-spec unsubscribe(atom(), handle()) -> ok.
unsubscribe(Name, Handle) ->
    gen_server:call(Name, {unsubscribe, Handle}).

-spec delete(atom()) -> ok.
delete(Name) ->
    gen_server:stop(Name).

%%====================================================================
%% Server Callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init([Capacity]) ->
    {ok, #state{capacity=Capacity}}.

-spec handle_cast({enqueue, any()}, state()) -> {noreply, state()}.
handle_cast({enqueue, Elem}, State) ->
    case enqueue_internal(State, Elem) of
        {ok, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end.

-spec handle_call(
        dequeue | {subscribe, func()} | {unsubscribe, handle()},
        any(),
        state()
       ) -> {reply, Reply, state()} 
                when Reply :: ok | full | empty | {value, any()}.
handle_call({enqueue, Elem}, _From, State) ->
    case enqueue_internal(State, Elem) of
        {ok, NewState} ->
            {reply, ok, NewState};
        _ ->
            {reply, full, State}
    end;
handle_call(dequeue, _From, State=#state{queue=Queue}) ->
    {R, Q} = queue:out(Queue),
    {reply, R, State#state{queue=Q}};
handle_call({subscribe, Func}, _From, State=#state{subscribers=Subs}) ->
    {reply, ok, State#state{subscribers=Subs#{make_ref() =>  Func}}};
handle_call({unsubscribe, Handle}, _From, State=#state{subscribers=Subs}) ->
    {reply, ok, State#state{subscribers=maps:remove(Handle, Subs)}}.

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

%%====================================================================
%% Private functions
%%====================================================================

-spec enqueue_internal(state(), any()) -> {ok, state()} | full.
enqueue_internal(State=#state{capacity=Cap, queue=Queue, subscribers=Subs}, Elem) ->
    case queue:len(Queue) < Cap of
        true ->
            [erlang:apply(M, F, A) || {M, F, A} <- maps:values(Subs)],
            {ok, State#state{queue=queue:in(Elem, Queue)}};
        _ ->
            full
    end.
