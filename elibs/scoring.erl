-module(scoring).

-compile(export_all).

-ifdef(TEST).
-include("etest/scoring_test.erl").
-endif.

% This function tries to score a partial details spec (Ref) vs. a full details spec (Details).
% @spec score_details(details(), details()) -> int()
score_details(Ref, Details) -> 
  RoleMatchScore = role_match_score(Ref, Details),
  VersionMatchScore = version_match_score(Ref, Details),
  TagBonusScore = tag_bonus_score(Ref, Details),
  RoleMatchScore + VersionMatchScore + TagBonusScore.
  
% Extract a specific key's contents from a details list.
% @spec extract(binary(), details()) -> nil() | [binary()]
extract(Key, Details) -> 
  case lists:keysearch(Key, 1, Details) of
    {value, {Key, V}} -> V ;
    false -> []
  end.
  
% Calculate role matching score. A perfect role match scores 0,
% anything else scores -1000
% @spec role_match_score(details(), details()) -> int()
role_match_score(Ref, Details) -> 
  RefRoles = extract(<<"roles">>, Ref),
  DetailsRoles = extract(<<"roles">>, Details),
  case lists:sort(RefRoles) =:= lists:sort(DetailsRoles) of
    true -> 0;
    false -> -1000
  end.
  
% Calculate version matching score. Matches get 1 points,
% conflicts get -1000
% @spec version_match_score(details(), details()) -> int()
version_match_score(Ref, Details) -> 
  FoldFunc = fun({K, V}, A) -> 
    case lists:keysearch(K, 1, Details) of
      false -> A;
      {value, {K, Value}} ->
        if 
          Value == V -> A + 1 ;
          true -> -1000
        end
    end
  end,
  lists:foldl(FoldFunc, 0, lists:keydelete(<<"tags">>, 1,  Ref)).

% Does the special tag calculation on a pair of details. Assists score_details.
% @spec calculate_tag_bonus(details(), details()) -> int()
tag_bonus_score(Ref, Details) -> 
  RefTags = extract(<<"tags">>, Ref),
  DetailsTags = extract(<<"tags">>, Details),
  length([X || X <- RefTags, lists:member(X, DetailsTags)]).