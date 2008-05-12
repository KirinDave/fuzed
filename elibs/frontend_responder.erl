-module(frontend_responder).
-include("yaws_api.hrl").
-include("yaws.hrl").
-include("fuzed.hrl").
-compile(export_all).

provide_pool(_A) ->
  resource_fountain:best_pool_for_details_match(details()).

out404(A, _GC, SC) ->
  Parameters = [{request, {struct, parse_arg(A, SC)}}],
  io:format("Param restructure:~n~p~n", [Parameters]),
  Pool = provide_pool(A),
  case node_api:safely_send_call_to_pool_no_lookup(handle_request,
                                                   Parameters, 
                                                   pure,
                                                   Pool) of
    {result, R} -> 
      convert_response(R);
    {error, R} ->
      error_logger:info_msg("500 Internal Server Error: ~p~n", [R]),
      [{status, 500}, {html, "Sumpin fucked."}]
  end.

parse_arg(Request, ServerOptions) ->
  Headers = Request#arg.headers,
  [convert_method(Request), 
   convert_version(Request), 
   convert_querypath(Request), 
   {querydata, prep(Request#arg.querydata)}, 
   {servername, prep(ServerOptions#sconf.servername)},
   {headers, {struct, convert_headers(Request#arg.headers)}},
   {cookies, {array, lists:map(fun(X) -> prep(X) end, Headers#headers.cookie)}},
   {pathinfo, prep(ServerOptions#sconf.docroot)},
   {postdata, Request#arg.clidata}].
    

convert_method(Request) ->
  R = Request#arg.req,
  {http_request,Method,{_Type,_Path},_} = R,
  {method, Method}.

convert_querypath(Request)  ->
   R = Request#arg.req,
   {http_request,_Method,{_Type,Path},_} = R,
   {querypath, prep(Path)}.

convert_version(Request) ->
  R = Request#arg.req,
  {http_request,_Method,{_Type,_Path},Version} = R,
  {http_version, {array, tuple_to_list(Version)}}.

convert_req(R) ->
  {http_request,Method,{_Type,Path},_} = R,
  {Method, prep(Path)}.

convert_headers(A) ->
  NormalHeaders = [{connection, prep(A#headers.connection)},
                   {accept, prep(A#headers.accept)},
                   {host, prep(A#headers.host)},
                   {if_modified_since, prep(A#headers.if_modified_since)},
                   {if_match, prep(A#headers.if_match)},
                   {if_none_match, prep(A#headers.if_none_match)},
                   {if_range, prep(A#headers.if_range)},
                   {if_unmodified_since, prep(A#headers.if_unmodified_since)},
                   {range, prep(A#headers.range)},
                   {referer, prep(A#headers.referer)},
                   {user_agent, prep(A#headers.user_agent)},
                   {accept_ranges, prep(A#headers.accept_ranges)},
                   {keep_alive, prep(A#headers.keep_alive)},
                   {location, prep(A#headers.location)},
                   {content_length, prep(A#headers.content_length)},
                   {content_type, prep(A#headers.content_type)},
                   {content_encoding, prep(A#headers.content_encoding)},
                   {authorization, prep(A#headers.authorization)},
                   {transfer_encoding, prep(A#headers.transfer_encoding)}],
  SpecialHeaders = 
    lists:map(fun({http_header, _Len, Name, _, Value}) -> {prep(Name), prep(Value)} end, 
              A#headers.other),
  [{Name, Res} || {Name, Res} <- NormalHeaders, Res /= undefined] ++ SpecialHeaders.

convert_response({response, EhtmlTuple}) ->
  {Status, AllHeaders, Html} = EhtmlTuple,
  {allheaders, HeaderList} = AllHeaders,
  ProcessedHeaderList = lists:map(fun({header, Name, Value}) -> {header, [binary_to_list(Name) ++ ":", binary_to_list(Value)]} end,
                                  tuple_to_list(HeaderList)),
  {html, RawResult} = Html,
  [Status, {allheaders, ProcessedHeaderList}, {html, binary_to_list(RawResult)}].

prep(A) when is_list(A) -> list_to_binary(A);
prep(A) -> A.

details() ->
  {ok, Details} = 
    application:get_env(fuzed_frontend, details),
  Details.
