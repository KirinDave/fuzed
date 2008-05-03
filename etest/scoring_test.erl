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
  
score_details_real_world_test() -> 
  Partial1 = [{<<"kind">>,<<"xle">>},{<<"grammar">>,<<"query">>}, {<<"roles">>, [<<"production">>]}],
  Partial2 = [{<<"kind">>,<<"xle">>},{<<"grammar">>,<<"query">>}],
  Complete = [{<<"tags">>,[]},
              {<<"rubygem_powerset_nl">>,{1,3,0,0}},
              {<<"rubygem_xle">>,{1,1,8,0}},
              {<<"rubygem_json">>,{1,1,0,9999}},
              {<<"pargram_english">>,{1,10,3,0}},
              {<<"powerset_rubygems">>,{0,9,2,9999}},
              {<<"xle">>,{2,3,8,0}},
              {<<"rubygem_json_rpc_client">>,{0,1,6,0}},
              {<<"name_tagger">>,{0,10,6,0}},
              {<<"powerset_ruby">>,{1,8,5,9999}},
              {<<"powerset_environment">>,{0,1,5,0}},
              {<<"xle_rpc">>,{0,3,4,32}},
              {<<"rubygem_thrift">>,{0,0,1,0}},
              {<<"thrift">>,{1,2,6,31}},
              {<<"onomasticon">>,{0,10,2,89}},
              {<<"c_fsm">>,{2,10,1,0}},
              {<<"lexicon_json_rpc_server">>,{0,10,4,0}},
              {<<"rubygem_json_rpc_server">>,{0,10,1,0}},
              {<<"rubygem_lexicon">>,{0,10,7,0}},
              {<<"lexicon_db">>,{0,10,1,0}},
              {<<"rubygem_mongrel">>,{1,0,1,9999}},
              {<<"rubygem_unicodechars">>,{0,0,2,9999}},
              {<<"queryreform">>,{0,2,33,0}},
              {<<"dictionary_server_rpc">>,{0,0,2,4}},
              {<<"rubygem_tokenizer">>,{0,0,8,18}},
              {<<"rubygem_xle_thrift_responder">>,{0,2,3,0}},
              {<<"rubygem_fastthread">>,{1,0,1,9999}},
              {<<"xleenv">>,{0,0,11,0}},
              {<<"prolog_lexicon_client">>,{0,1,22,0}},
              {<<"sicstus">>,{3,12,7,0}},
              {<<"zlib">>,{1,2,3,0}},
              {<<"tk">>,{8,4,14,0}},
              {<<"libiconv">>,{1,11,4}},
              {<<"tcl">>,{8,4,14,0}},
              {<<"rubygem_rspec">>,{1,0,8,9999}},
              {<<"rubygem_hapnis">>,{0,10,0,0}},
              {<<"rubygem_alias">>,{0,10,0,0}},
              {<<"powerset_ruby_bdb">>,{0,6,0,9999}},
              {<<"rubygem_libxml_ruby">>,{0,3,8,9999}},
              {<<"rubygem_sores">>,{0,10,2,0}},
              {<<"rubygem_unidecode">>,{1,0,0,9999}},
              {<<"ps_semantics">>,{0,8,2,0}},
              {<<"db4">>,{4,3,29,0}},
              {<<"ps_semantics_rule_induction">>,{0,8,2,0}},
              {<<"ps_semantics_base">>,{0,8,2,0}},
              {<<"tadm">>,{0,9,8,6}},
              {<<"powerset_python_module_environment">>,{0,0,1,0}},
              {<<"fst_tag_jsonrpc">>,{0,1,1,0}},
              {<<"python_jsonrpc_client">>,{1,0,2,0}},
              {<<"python_jsonrpc_server">>,{0,0,6,0}},
              {<<"py_fsm">>,{0,1,0,0}},
              {<<"roles">>,[<<"production">>]},
              {<<"kind">>,<<"xle">>},
              {<<"grammar">>,<<"query">>}],
  3 = score_details(Partial1, Partial1),
  3 = score_details(Partial1, Complete),
  0 > score_details(Partial2, Complete).
  


% extract

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