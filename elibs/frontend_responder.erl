-module(frontend_responder).
-include("responder_base.erl").
-compile(export_all).

provide_pool(_A) ->
  resource_fountain:best_pool_for_details_match(details()).
