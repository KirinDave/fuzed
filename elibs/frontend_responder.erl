-module(frontend_responder).
-include("responder_base.erl").
-compile(export_all).

% This responder can modify the request, and is obligated to return
% a pool and the request. You can just return it directly if you want.
provide_pool(A, _GC, _SC) ->
  {resource_fountain:best_pool_for_details_match(details()), A}.
