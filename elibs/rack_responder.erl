%% @author Abhay Kumar <abhay@opensynapse.net>

-module(rack_responder).
-author("Abhay Kumar <abhay@opensynapse.net>").
-export([handler/2]).

-include_lib("kernel/include/file.hrl").

handler(Req, DocRoot) ->
  try handle_static(Req, DocRoot)
  catch
    error:non_static -> handle_non_static(Req, DocRoot)
  end.

handle_static(Req, DocRoot) ->
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

handle_non_static(Req, DocRoot) ->
  case resource_fountain:best_pool_for_details_match(get_details()) of
    none -> Req:respond({503, [], "No pool to fufill request!"});
    Pool ->
      Parameters = [{request, {struct, parse_request(Req, DocRoot)}}],
      case node_api:safely_send_call_to_pool_no_lookup(handle_request, Parameters, pure, Pool) of
        {result, Resp} -> Req:respond(process_response(Resp));
        {error, Resp} ->
          error_logger:info_msg("500 Internal Server Error: ~p~n", [Resp]),
          Req:respond({500, [], "Internal Server Error due to failed response."})
      end
  end.

get_details() ->
  {ok, Details} = application:get_env(fuzed_frontend, details),
  Details.

parse_request(Req, DocRoot) ->
  RawPath = Req:get(raw_path),
  QueryString = extract_query_data(RawPath),
  [
    {method, Req:get(method)},
    {http_version, {array, tuple_to_list(Req:get(version))}},
    {querypath, list_to_binary(RawPath)},
    {querydata, list_to_binary(QueryString)},
    {servername, list_to_binary(atom_to_list(?MODULE))},
    {headers, {struct, process_headers(mochiweb_headers:to_list(Req:get(headers)))}},
    {cookies, {array, process_cookies(Req:parse_cookie())}},
    {pathinfo, list_to_binary(DocRoot)},
    {postdata, Req:recv_body()}
  ].

extract_query_data(RawPath) ->
  {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
  QueryString.

process_headers(Headers) when is_list(Headers)->
  [{process_key(K), list_to_binary(V)} || {K, V} <- Headers].

process_key(K) when is_atom(K) ->
  process_key(atom_to_list(K));
process_key(K) when is_list(K) ->
  Downcased = string:to_lower(K),
  case regexp:gsub(Downcased, "-", "_") of
    {ok, Cleaned, _Count} -> list_to_atom(Cleaned);
    _ -> list_to_atom(Downcased)
  end.

process_cookies(CookieList) when is_list(CookieList) ->
  [list_to_binary(lists:flatten(Key ++ "=" ++ Value)) || {Key, Value} <- CookieList].

process_response({response, EhtmlTuple}) ->
  {{status, Status}, {allheaders, HeaderList}, {html, RawResult}} = EhtmlTuple,
  ProcessedHeaderList = lists:map(
    fun({header, Name, Value}) ->
      {header, [binary_to_list(Name) ++ ":", binary_to_list(Value)]}
    end,
    tuple_to_list(HeaderList)
  ),
  {Status, ProcessedHeaderList, binary_to_list(RawResult)}.
