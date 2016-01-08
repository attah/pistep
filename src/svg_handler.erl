-module(svg_handler).
-behaviour(cowboy_http_handler).
-export([init/3,handle/2,terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, Opts) ->
  svg_keeper ! {get,self()},
  Svg = receive 
    {svg,S} ->
      S
  after 
    1337 ->
      []
  end,
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"image/svg+xml">>}], Svg, Req),
  {ok, Req2, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
