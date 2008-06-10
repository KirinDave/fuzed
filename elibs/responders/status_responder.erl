-module(status_responder).
-include("yaws_api.hrl").
-export([out/1, mochiweb_handler/1]).

%% START Yaws Specific Stuff
out(Arg) ->
  get_status(extract_path_info(Arg#arg.appmoddata)).
%% END Yaws Specific Stuff

%% START Mochiweb Specific Stuff
mochiweb_handler(Req) ->
  "/status" ++ RestPath = Req:get(path),
  [{status, Status}, {html, Message}] = get_status(extract_path_info(RestPath)),
  Req:respond({Status, [], Message}).
%% END Mochiweb Specific Stuff

get_status(PathInfo) ->
  case application:get_env(fuzed, in_rotation) of
    {ok, true} ->
      case PathInfo of
        any ->
          {Status, Message} = check_pools(resource_fountain:details_list()),
          [{status, Status}, {html, Message}];
        nodes -> [{status, 200}, {html, io_lib:format("Nodes: ~p", [nodes()])}];
        {error, Message} -> [{status, 400}, {html, "There was some error in your request. " ++ Message}];
        {Kind, Min} ->
          {Status, Message} = check_pools(resource_fountain:details_list(), Kind, Min),
          [{status, Status}, {html, Message}]
      end;
    _ -> [{status, 500}, {html, "This master is not published."}]
  end.

extract_path_info(Path) ->
  case string:tokens(Path, "/") of
    []          -> any;
    ["nodes"]   -> nodes;
    [Kind]      -> {Kind, 1};
    [Kind, Min] ->
      try {Kind, list_to_integer(Min)}
      catch
        _:_ -> {error, "Request URI had malformed number."}
      end;
    _ -> {error, "Request URI had more than two sections."}
  end.

check_pools(PoolList) when is_list(PoolList) ->
  case length(PoolList) of
    0 -> {500, "There are no pools attached to this master."};
    NotZero ->
      NumWorkers = length(workers_for_pools(PoolList)),
      {200, io_lib:format("There are ~p pools attached to this master with a total of ~p workers.", [NotZero, NumWorkers])}
  end.

check_pools(PoolList, Kind, Min) when is_list(Kind) -> check_pools(PoolList, list_to_binary(Kind), Min);
check_pools(PoolList, Kind, Min) when is_binary(Kind) ->
  Pred = fun(X) ->
    case lists:keysearch(<<"kind">>, 1, X) of
      {value, {_,Kind}} -> true;
      _                 -> false
    end
  end,
  MatchingPools = lists:filter(Pred, PoolList),
  NumMatchingPools = length(MatchingPools),
  NumWorkers = length(workers_for_pools(MatchingPools)),
  case NumWorkers < Min of
    false -> {200, io_lib:format("There are ~p ~s pool(s) attached to this master with a total of ~p worker(s).", [NumMatchingPools, Kind, NumWorkers])};
    true  -> {500, io_lib:format("Expected at least ~p worker(s) in the ~p ~s pool(s). Found ~p. ", [Min, NumMatchingPools, Kind, NumWorkers])}
  end.

workers_for_pools(PoolList) ->
  Pools = [resource_fountain:pool_for_details(X) || X <- PoolList],
  lists:flatten([resource_pool:list(S) || S <- Pools]).
