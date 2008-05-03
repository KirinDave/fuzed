-include_lib("eunit/eunit.hrl").

% best_details_match

best_details_match_should_return_best_match_test() ->
  Dict0 = dict:new(),
  Dict1 = dict:store([{<<"alpha">>, <<"1.2.3">>}], a, Dict0),
  Dict2 = dict:store([{<<"beta">>, <<"5.5.5">>}], b, Dict1),
  Dict3 = dict:store([{<<"alpha">>, <<"1.2.3">>}, {<<"beta">>, <<"5.5.5">>}], c, Dict2),
  Ref = [{<<"alpha">>, <<"1.2.3">>}, {<<"beta">>, <<"5.5.5">>}],
  {2, Ref} = best_details_match(Ref, Dict3).
