-include_lib("eunit/eunit.hrl").

% HELPERS %

% atom_to_binary
atom_to_binary_test() ->
  <<"hello">> = atom_to_binary(hello).

% normalize_details_tuple
ndt_basic_test() ->
  [DTs, DTi, DTm] = [{bears, "1.2.3-1"}, {beets, 0}, {bg, "1.2.3"}],
  {<<"bears">>, {1, 2, 3, 1}} = normalize_detail_tuple(DTs),
  {<<"beets">>, <<"0">>} = normalize_detail_tuple(DTi),
  {<<"bg">>, {1,2,3}} = normalize_detail_tuple(DTm).

ndt_tags_test() ->
  [_DTs, _DTi, DTt] = [{bears, "yes"}, {beets, 0}, {tags, "one,two,three"}],
  {<<"tags">>, [<<"one">>, <<"two">>, <<"three">>]} = normalize_detail_tuple(DTt).

% uniquify_list
uql_test_test() ->
  L2 = [shinagami, love, apples],
  L1 = [apples, love, apples, love, shinagami],
  L2 = uniquify_list(L1).

% k()
k_does_basics_test() ->
  L1 = [{bears, "Hello!"}, {beets, "WORLD!"}],
  "Hello!" = k(bears, L1),
  "WORLD!" = k(beets, L1),
  false = k(bsg, L1).

k_does_term_translation_test() ->
  L1 = [{bears, "Hello!"}, {beets, "WORLD!"}],
  "Hello!" = k("bears", L1),
  false = k("bsg", L1).

k_does_details_right_test() ->
  Details = [{xle, "1.2.3-4"}, {tags, "one"}],
  L1 = [{details, {struct, Details}}, {notdetails, "Fakeness"}],
  [{<<"xle">>, {1,2,3,4}}, {<<"tags">>, [<<"one">>]}] = k(details, L1).


