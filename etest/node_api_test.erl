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
  
