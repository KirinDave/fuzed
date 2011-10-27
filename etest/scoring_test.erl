-include_lib("include/eunit/eunit.hrl").

% score_details

score_details_should_return_0_for_empty_details_test() ->
  0 = score_details([], []).

score_details_should_return_1_for_single_tag_match_test() ->
  D1 = [{<<"tags">>, [<<"foobar">>]}],
  1 = score_details(D1, D1).

score_details_should_return_n_for_n_tag_match_test() ->
  D1 = [{<<"tags">>, [<<"foo">>, <<"bar">>, <<"baz">>]}],
  3 = score_details(D1, D1).

score_details_should_return_1_for_single_version_match_test() ->
  D1 = [{<<"alpha">>, <<"1.2.3">>}],
  D2 = [{<<"alpha">>, <<"1.2.3">>}, {<<"beta">>, <<"5.5.5">>}],
  1 = score_details(D1, D1),
  1 = score_details(D1, D2).

score_details_should_return_2_for_mixed_type_test() ->
  D1 = [{<<"alpha">>, <<"1.2.3">>}, {<<"gramma">>, <<"query">>}],
  D2 = [{<<"alpha">>, <<"1.2.3">>}, {<<"beta">>, <<"5.5.5">>}, {<<"gramma">>, <<"query">>}],
  2 = score_details(D1, D1),
  2 = score_details(D1, D2).

score_details_should_return_minus_1000_for_non_match_test() ->
  D1 = [{<<"alpha">>, <<"1.2.3">>}],
  D2 = [{<<"alpha">>, <<"3.2.1">>}],
  -1000 = score_details(D1, D2).


extract_should_extract_single_tag_list_test() ->
  Tag = extract(<<"tags">>, [{<<"tags">>, [<<"foobar">>]}]),
  [<<"foobar">>] = Tag.

extract_should_extract_multiple_tag_list_test() ->
  Tag = extract(<<"tags">>, [{<<"tags">>, [<<"foo">>, <<"bar">>]}]),
  [<<"foo">>, <<"bar">>] = Tag.

extract_should_return_nil_if_not_matching_tags_test() ->
  Tag = extract(<<"tags">>, [{<<"bogus">>, [<<"foobar">>]}]),
  [] = Tag.

% role_match_score

role_match_score_should_return_0_for_perfect_match_test() ->
  D0 = [],
  D1 = [{<<"roles">>, [<<"foo">>]}],
  D2 = [{<<"roles">>, [<<"foo">>, <<"bar">>]}],
  0 = role_match_score(D0, D0),
  0 = role_match_score(D1, D1),
  0 = role_match_score(D2, D2).

role_match_score_should_return_minus_1000_for_non_match_test() ->
  D1 = [{<<"roles">>, [<<"foo">>]}],
  D2 = [{<<"roles">>, [<<"foo">>, <<"bar">>]}],
  -1000 = role_match_score(D1, D2),
  -1000 = role_match_score(D2, D1).

% tag_bonus_score

tag_bonus_score_should_give_one_point_for_each_match_test() ->
  D1 = [{<<"tags">>, [<<"foo">>, <<"bar">>, <<"baz">>]}],
  D2 = [{<<"tags">>, [<<"foo">>, <<"bar">>]}],
  D3 = [{<<"tags">>, [<<"foo">>, <<"qux">>]}],
  D4 = [{<<"tags">>, [<<"qux">>, <<"quux">>]}],
  D5 = [{<<"tags">>, []}],
  0 = tag_bonus_score(D1, D5),
  0 = tag_bonus_score(D4, D1),
  1 = tag_bonus_score(D3, D1),
  2 = tag_bonus_score(D2, D1),
  3 = tag_bonus_score(D1, D1).
