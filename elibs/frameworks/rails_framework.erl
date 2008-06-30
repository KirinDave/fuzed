-module(rails_framework).
-export([crashmsg/3]).
-include("yaws_api.hrl").
-include("yaws.hrl").

crashmsg(Arg, _SC, Str) ->
  Docroot = Arg#arg.docroot,
  Path500 = filename:join([Docroot, "500.html"]),
  case file:read_file(Path500) of
    {ok, Binary} ->
      {html, Binary};
    {error, _Reason} ->
      {html, ["<pre>", Str, "</pre>"]}
  end.