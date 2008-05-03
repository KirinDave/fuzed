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

jsonifying_details_test() ->
  BadTerm = [{<<"tags">>,[]},
   {<<"boost_filesystem">>,{1,34,1,0}},
   {<<"boost_program_options">>,{1,34,1,0}},
   {<<"boost_regex">>,{1,34,1,0}},
   {<<"dictionary_server_rpc">>,{0,0,2,3}},
   {<<"rubygem_json">>,{1,1,0,9999}},
   {<<"rubygem_powerset_nl">>,{1,2,23,1}},
   {<<"rubygem_xle">>,{1,1,7,0}},
   {<<"dictionary_server">>,{0,0,4,5}},
   {<<"log4cpp">>,{0,3,5,5}},
   {<<"onomasticon">>,{0,6,6,0}},
   {<<"powerset_python_module_environment">>,{0,0,1,0}},
   {<<"ps_semantics_rule_induction">>,{0,6,12,0}},
   {<<"tokenizer">>,{0,0,6,12}},
   {<<"xle_rpc">>,{0,3,4,38}},
   {<<"lexicon_json_rpc_server">>,{0,6,2,0}},
   {<<"libiconv">>,{1,11,0}},
   {<<"pargram_english">>,{1,7,1,0}},
   {<<"ps_semantics_base">>,{0,6,12,0}},
   {<<"queryreform">>,{0,2,30,0}},
   {<<"rubygem_gem_plugin">>,{0,2,2,9999}},
   {<<"rubygem_tokenizer">>,{0,0,8,19}},
   {<<"rubygem_xle_thrift_responder">>,{0,2,3,0}},
   {<<"boost">>,{1,34,1,0}},
   {<<"cxx">>,{0,2,4,44}},
   {<<"libevent">>,{1,3,2}},
   {<<"rubygem_rspec">>,{0,9,3,9999}},
   {<<"rubygem_thrift">>,{0,0,1,0}},
   {<<"powerset_rubygems">>,{0,9,2,9999}},
   {<<"prolog_lexicon_client">>,{0,1,19,0}},
   {<<"rubygem_activerecord">>,{1,15,3,9999}},
   {<<"rubygem_daemons">>,{1,0,8,9999}},
   {<<"rubygem_fastthread">>,{1,0,1,9999}},
   {<<"sicstus">>,{3,12,7,0}},
   {<<"boost_thread">>,{1,34,1,0}},
   {<<"dictionary_loader">>,{0,0,2,3}},
   {<<"log4cpp-devel">>,{0,3,5,5}},
   {<<"rubygem_facets">>,{1,8,54,9999}},
   {<<"server_base">>,{0,2,0,67}},
   {<<"xle">>,{2,3,2,0}},
   {<<"boost-devel">>,{1,34,1,0}},
   {<<"dictionary_db">>,{0,0,0,0}},
   {<<"powerset_ruby_bdb">>,{0,6,0,9999}},
   {<<"ps_semantics_nl_match">>,{0,1,3,0}},
   {<<"rubygem_json_rpc_service">>,{0,5,4,0}},
   {<<"rubygem_lexicon">>,{0,6,14,0}},
   {<<"rubygem_libxml_ruby">>,{0,3,8,9999}},
   {<<"thrift">>,{1,2,0,26}},
   {<<"c_fsm">>,{2,10,1,0}},
   {<<"rubygem_rake">>,{0,7,3,9999}},
   {<<"lexicon_db">>,{0,6,7,0}},
   {<<"rubygem_activesupport">>,{1,4,4,9999}},
   {<<"rubygem_cgi_multipart_eof_fix">>,{2,1,9999}},
   {<<"rubygem_mongrel">>,{1,0,1,9999}},
   {<<"scribe_rpc">>,{0,0,5,4}},
   {<<"tbb">>,{2,0,0,5}},
   {<<"boost_iostreams">>,{1,34,1,0}},
   {<<"powerset_environment">>,{0,1,5,0}},
   {<<"powerset_ruby">>,{1,8,5,9999}},
   {<<"ps_semantics">>,{0,6,12,0}},
   {<<"ps_semantics_procedural_attachments">>,{0,1,8,0}},
   {<<"ps_semantics_semxml">>,{0,1,3,0}},
   {<<"rubygem_unicodechars">>,{0,0,2,9999}},
   {<<"xleenv">>,{0,0,11,0}},
   {<<"nl_runtime_stack">>,{1,7,6,22}},
   {<<"grammar">>,<<"query">>}],
  jsonify_full_details_list(BadTerm),
  Details = [{<<"bears">>, {1,2,3,4}}, {<<"beats">>, <<"funky">>}, {<<"tags">>, [<<"one">>]}],
  {struct, [{"bears","1.2.3-4"},
   {"beats","funky"},
   {"tags",{array, ["one"]}}]} = jsonify_full_details_list(Details).
 

