%%%-------------------------------------------------------------------
%% @doc webrtc public API
%% @end
%%%-------------------------------------------------------------------

-module(webrtc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs = [Port]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, webrtc, "index.html"}},
            {"/client", cowboy_static, {priv_file, webrtc, "client.html"}},
            {"/server", cowboy_static, {priv_file, webrtc, "server.html"}},
            {"/websocket", ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, webrtc, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]),
    webrtc_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
