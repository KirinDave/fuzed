-include_lib("eunit/eunit.hrl").

basic_conversion_test() -> 
  V1 = [{passage, "This is my boomstick"},     {options, ""}],
  V2 = [{passage, <<"This is my boomstick">>}, {options, <<"">>}],
  V2 = prepare_parameters(V1).
  
  
array_conversion_test() -> 
  V1 = [{key, {array, [<<"value1">>, <<"value2">>]}}],
  V2 = [{key, {array, [<<"value1">>, <<"value2">>]}}],
  V2 = prepare_parameters(V1).
  
struct_converstion_test() -> 
  V1 = [{options, {struct, [{restrict_to_url, "Test me!"}]}}],
  V2 = [{options, {struct, [{restrict_to_url, <<"Test me!">>}]}}],
  V2 = prepare_parameters(V1).

nested_struct_array_conversion_test() -> 
  V1 = [{options, 
          {struct, [
            {restrict_to_url, {struct, [{key, <<"Test me!">>}]}}]}}],
  V2 = [{options, 
          {struct, [
            {restrict_to_url, {struct, [{key, <<"Test me!">>}]}}]}}],
  V2 = prepare_parameters(V1).
  
realistic_parameter_test() -> 
  V1 = [{options,{struct,[{sem_laxer_pseudo_coefficient,"9"},
                                {sem_loose_default_role_coeff,"1.0"},
                                {sem_semor_expand_hierarchy,"0.5"},
                                {title_redirect_score,"0.0"},
                                {kw_max_keyphrase_length,"5"},
                                {kw_keyphrase_full_query,"true"},
                                {kw_pseudo_stopword_relative_weight,"0.7"}]}},
              {passage,"Why doesn't this work?"}],
  V2 = [{options,{struct,[{sem_laxer_pseudo_coefficient,<<"9">>},
                          {sem_loose_default_role_coeff,<<"1.0">>},
                          {sem_semor_expand_hierarchy,<<"0.5">>},
                          {title_redirect_score,<<"0.0">>},
                          {kw_max_keyphrase_length,<<"5">>},
                          {kw_keyphrase_full_query,<<"true">>},
                          {kw_pseudo_stopword_relative_weight,<<"0.7">>}]}},
              {passage,<<"Why doesn't this work?">>}],
  V2 = prepare_parameters(V1).
        
