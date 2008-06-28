-export([out/3, out404/3]).
-include("yaws_api.hrl").
-include("yaws.hrl").
-include("fuzed.hrl").
-include_lib("kernel/include/file.hrl").


%% START Yaws Specific Stuff
out(Arg, GC, SC) ->
  out404(Arg, GC, SC).

out404(Arg, GC, SC) ->
  Pool = provide_pool(),
  yaws_process_request(Pool, Arg, GC, SC).

yaws_process_request(none, _Arg, _GC, _SC) ->
  [{status, 503}, {html, "There is no pool to field your request."}];
yaws_process_request(Pool, Arg, _GC, SC) ->
  Parameters = [{request, {struct, yaws_process_arg(Arg, SC)}}],
  case execute_request(Pool, Parameters) of
    {Status, [], Message} -> [{status, Status}, {html, Message}];
    {Status, Headers, Message} -> [{status, Status}, {allheaders, Headers}, {html, Message}]
  end.

yaws_process_arg(Arg, SC) ->
  Headers = Arg#arg.headers,
  [{method, yaws_prepare(method, Arg)},
   {http_version, yaws_prepare(http_version, Arg)},
   {https, determine_ssl(SC)},
   {remote_addr, yaws_prepare(remote_addr, Arg)},
   {querypath, yaws_prepare(querypath, Arg)},
   {querydata, prep(Arg#arg.querydata)}, 
   {servername, prep(SC#sconf.servername)},
   {headers, {struct, yaws_prepare_headers(Headers)}},
   {cookies, {array, lists:map(fun(X) -> prep(X) end, Headers#headers.cookie)}},
   {pathinfo, prep(SC#sconf.docroot)},
   {postdata, Arg#arg.clidata}].
  
yaws_prepare(method, Arg) ->
  {http_request, Method, {_Type, _Path}, _Version} = Arg#arg.req,
  Method;
yaws_prepare(http_version, Arg) ->
  {http_request, _Method, {_Type, _Path}, Version} = Arg#arg.req,
  {array, tuple_to_list(Version)};
yaws_prepare(querypath, Arg) ->
  {http_request, _Method, {_Type, Path}, _Version} = Arg#arg.req,
  prep(Path);
yaws_prepare(remote_addr, Arg) ->
  Socket = Arg#arg.clisock,
  try
    get_remote_addr(Socket)
  catch
    _:_ -> ok
  end.
  
get_remote_addr(Socket) ->
  Peer = inet:peername(Socket),
  case Peer of
    {ok, {AddressIntegerTuple, _Port}} ->
      AddressIntegerList = tuple_to_list(AddressIntegerTuple),
      AddressStringList = lists:map(fun(X) -> integer_to_list(X) end, AddressIntegerList),
      Address = string:join(AddressStringList, "."),
      prep(Address);
    _Else ->
      "0.0.0.0"
  end.

yaws_prepare_headers(Headers) ->
  NormalHeaders = [{connection, prep(Headers#headers.connection)},
                   {accept, prep(Headers#headers.accept)},
                   {host, prep(Headers#headers.host)},
                   {if_modified_since, prep(Headers#headers.if_modified_since)},
                   {if_match, prep(Headers#headers.if_match)},
                   {if_none_match, prep(Headers#headers.if_none_match)},
                   {if_range, prep(Headers#headers.if_range)},
                   {if_unmodified_since, prep(Headers#headers.if_unmodified_since)},
                   {range, prep(Headers#headers.range)},
                   {referer, prep(Headers#headers.referer)},
                   {user_agent, prep(Headers#headers.user_agent)},
                   {accept_ranges, prep(Headers#headers.accept_ranges)},
                   {keep_alive, prep(Headers#headers.keep_alive)},
                   {location, prep(Headers#headers.location)},
                   {content_length, prep(Headers#headers.content_length)},
                   {content_type, prep(Headers#headers.content_type)},
                   {content_encoding, prep(Headers#headers.content_encoding)},
                   {authorization, prep_authorization(Headers#headers.authorization)},
                   {transfer_encoding, prep(Headers#headers.transfer_encoding)}],
  SpecialHeaders = 
    lists:map(fun({http_header, _Len, Name, _, Value}) -> {prep(Name), prep(Value)} end, 
              Headers#headers.other),
  [{Name, Res} || {Name, Res} <- NormalHeaders, Res /= undefined] ++ SpecialHeaders.
%% END Yaws Specific Stuff

%% START Mochiweb Specific Stuff
mochiweb_handler(Req, DocRoot) ->
  try mochiweb_handle_static(Req, DocRoot)
  catch
    error:non_static -> mochiweb_handle_non_static(Req, DocRoot)
  end.

mochiweb_handle_static(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  case mochiweb_util:safe_relative_path(Path) of
    undefined -> erlang:error(non_static);
    RelPath ->
      FullPath = filename:join([DocRoot, RelPath]),
      File = case filelib:is_dir(FullPath) of
        true -> filename:join([FullPath, "index.html"]);
        false -> FullPath
      end,
      case file:read_file_info(File) of
        {ok, FileInfo} ->
          LastModified = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
          case Req:get_header_value("if-modified-since") of
            LastModified -> Req:respond({304, [], ""});
            _ ->
              case file:open(File, [raw, binary]) of
                {ok, IoDevice} ->
                  ContentType = mochiweb_util:guess_mime(File),
                  Res = Req:ok({ContentType, [{"last-modified", LastModified}], {file, IoDevice}}),
                  file:close(IoDevice),
                  Res;
                _ -> erlang:error(non_static)
              end
          end;
        {error, _} -> erlang:error(non_static)
      end
  end.

mochiweb_handle_non_static(Req, DocRoot) ->
  Pool = provide_pool(),
  mochiweb_process_non_static(Pool, Req, DocRoot).

mochiweb_process_non_static(none, Req, _DocRoot) ->
  Req:respond({503, [], "There is no pool to field your request."});
mochiweb_process_non_static(Pool, Req, DocRoot) ->
  Parameters = [{request, {struct, mochiweb_parse_request(Req, DocRoot)}}],
  Req:respond(execute_request(Pool, Parameters)).

mochiweb_parse_request(Req, DocRoot) ->
  RawPath = Req:get(raw_path),
  {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
  Cookies = [list_to_binary(lists:flatten(Key ++ "=" ++ Value)) || {Key, Value} <- Req:parse_cookie()],
  Headers = mochiweb_prepare_headers(mochiweb_headers:to_list(Req:get(headers))),
  
  [{method, Req:get(method)},
   {http_version, {array, tuple_to_list(Req:get(version))}},
   {querypath, list_to_binary(RawPath)},
   {querydata, list_to_binary(QueryString)},
   {servername, list_to_binary(atom_to_list(?MODULE))},
   {headers, {struct, Headers}},
   {cookies, {array, Cookies}},
   {pathinfo, list_to_binary(DocRoot)},
   {postdata, Req:recv_body()}
  ].

mochiweb_prepare_headers(Headers) when is_list(Headers) ->
  [{mochiweb_process_key(K), list_to_binary(V)} || {K, V} <- Headers].

mochiweb_process_key(K) when is_atom(K) ->
  mochiweb_process_key(atom_to_list(K));
mochiweb_process_key(K) when is_list(K) ->
  Downcased = string:to_lower(K),
  case regexp:gsub(Downcased, "-", "_") of
    {ok, Cleaned, _Count} -> list_to_atom(Cleaned);
    _ -> list_to_atom(Downcased)
  end.

%% END Mochiweb Specific Stuff

determine_ssl(SC) ->
  case SC#sconf.ssl of
    undefined -> 0;
    _Else -> 1
  end.
  
execute_request(Pool, Parameters) ->
  case node_api:safely_send_call_to_pool_no_lookup(handle_request, Parameters, pure, Pool) of
    {result, Result} -> result_processor(Result);
    {error, Result} ->
      error_logger:info_msg("500 Internal Server Error: ~p~n", [Result]),
      {500, [], "Internal Server Error due to failed response."}
  end.

result_processor({response, {{status, Status}, {allheaders, HeaderTuple}, {html, ResultBinary}}}) ->
  ProcessedHeaderList = lists:map(fun({header, Name, Value}) -> {header, [binary_to_list(Name) ++ ":", binary_to_list(Value)]} end, tuple_to_list(HeaderTuple)),
  {Status, ProcessedHeaderList, binary_to_list(ResultBinary)}.

prep_authorization({_User, _Pass, Auth}) ->
  list_to_binary(Auth);
prep_authorization(Any) ->
  Any.

details() ->
  {ok, Details} = application:get_env(fuzed_frontend, details),
  Details.

prep(A) when is_list(A) -> list_to_binary(A);
prep(A) -> A.
