-module(erlbq).
-behaviour(gen_server).

%% API exports
-export([
    %% api
    new/1,
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
-type id() :: atom() | pid().

-export_type([
    handle/0,
    id/0
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

-spec new(integer()) -> {ok, id()} | {error, any()}.
new(Capacity) ->
    gen_server:start_link(?MODULE, [Capacity], []).

-spec new(atom(), integer()) -> {ok, id()} | {error, any()}.
new(Name, Capacity) ->
    gen_server:start_link({local, Name}, ?MODULE, [Capacity], []).

-spec enqueue(id(), any()) -> ok | full.
enqueue(Id, Elem) ->
    gen_server:call(Id, {enqueue, Elem}).

-spec enqueue_cast(id(), any()) -> ok.
enqueue_cast(Id, Elem) ->
    gen_server:cast(Id, {enqueue, Elem}).

-spec dequeue(id()) -> {ok, any()} | empty.
dequeue(Id) ->
    gen_server:call(Id, dequeue).

-spec subscribe(id(), func()) -> handle().
subscribe(Id, Func) ->
    gen_server:call(Id, {subscribe, Func}).

-spec unsubscribe(id(), handle()) -> ok.
unsubscribe(Id, Handle) ->
    gen_server:call(Id, {unsubscribe, Handle}).

-spec delete(id()) -> ok.
delete(Id) ->
    gen_server:stop(Id).

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

-spec handle_call({enqueue, any()}, any(), state()) ->
                         {reply, ok | full, state()};
                 ({dequeue, handle()}, any(), state()) ->
                         {reply, empty | {value, any()}, state()};
                 ({subscribe, func()}, any(), state()) ->
                         {reply, handle(), state()};
                 ({unsubscribe, handle()}, any(), state())->
                         {reply, ok, state()}.
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
    Handle = make_ref(),
    {reply, Handle, State#state{subscribers=Subs#{Handle =>  Func}}};
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
enqueue_internal(
  State=#state{capacity=Cap, queue=Queue, subscribers=Subs},
  Elem
 ) ->
    case queue:len(Queue) < Cap of
        true ->
            [erlang:apply(M, F, A) || {M, F, A} <- maps:values(Subs)],
            {ok, State#state{queue=queue:in(Elem, Queue)}};
        _ ->
            full
    end.
