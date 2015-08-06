-module(pistep_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([{'_', [
        {"/", cowboy_static, {priv_file, pistep, "static/index.html"}},
        {"/debug.svg", cowboy_static, {priv_file, pistep, "debug.svg"}},
        {"/style.css", cowboy_static, {priv_file, pistep, "static/style.css"}},
        {"/connect", ws_handler, []} 
        ]}]),
        Port = 8080,
        cowboy:start_http(my_http_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
        pistep:start(),   
	pistep_sup:start_link().

stop(_State) ->
	ok.
