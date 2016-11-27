%%%-------------------------------------------------------------------
%% @doc webrtc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(webrtc_sup).

-behaviour(supervisor).

-include("../include/webrtc.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [#{
        id => ?SESSION_STORAGE,
        start => {?SESSION_STORAGE, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [?SESSION_STORAGE]
    }]} }.

%%====================================================================
%% Internal functions
%%====================================================================
