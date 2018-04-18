-module(erlbq_eunit).

-include_lib("eunit/include/eunit.hrl").

-export([
    sub_unsub_test_helper/1
  ]).

enq_deq_test_() ->
    with_server(
      fun(Id) ->
              erlbq:enqueue(Id, something),
              ?_assertEqual({value, something}, erlbq:dequeue(Id))
      end
     ).

empty_deq_test_() ->
    with_server(
      fun(Id) ->
              ?_assertEqual(empty, erlbq:dequeue(Id))
      end
     ).

full_enq_test_() ->
    with_server(10,
      fun(Id) ->
              [erlbq:enqueue(Id, Val) || Val <- lists:seq(0, 9)],
              ?_assertEqual(full, erlbq:enqueue(Id, 10))
      end
     ).

sub_unsub_test_() ->
    with_server(
      fun(Id) ->
              Tbl = ets:new(?MODULE, [set, public]),
              Handle =
                  erlbq:subscribe(Id, {?MODULE, sub_unsub_test_helper, [Tbl]}),
              true = ets:insert(Tbl, {notification_count, 0}),
              ok = erlbq:enqueue(Id, elem1),
              AfterEnqueue1 = ets:lookup_element(Tbl, notification_count, 2),
              erlbq:unsubscribe(Id, Handle),
              ok = erlbq:enqueue(Id, elem2),
              AfterEnqueue2 = ets:lookup_element(Tbl, notification_count, 2),
              ets:delete(Tbl),
              [?_assertEqual(1, AfterEnqueue1),
               ?_assertEqual(1, AfterEnqueue2)]
      end
     ).

sub_unsub_test_helper(Tbl) ->
    ets:update_counter(Tbl, notification_count, 1).

with_server(Function) ->
    with_server(10, Function).

with_server(Cap, Function) ->
    with_server(undefined, Cap, Function).

with_server(Name, Cap, Function) ->
    {setup,
     fun () ->
             {ok, Id} =
                 case Name of
                     undefined ->
                         erlbq:new(Cap);
                     _ ->
                         erlbq:new(Name, Cap)
                 end,
             Id
     end,
     fun (Id) -> erlbq:delete(Id) end,
     Function
    }.
