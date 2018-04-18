-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

proper_test_() ->
    [{atom_to_list(F),
      {timeout, 120,
       fun () -> ?assert(
                    io:format(standard_error, "~p~n", [F]),
                    proper:quickcheck(
                      ?MODULE:F(),
                      [long_result, {to_file, user}])
                   )
       end
      }
     }
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].
