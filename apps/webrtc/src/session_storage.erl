%%%-------------------------------------------------------------------
%%% @author huangcheng
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2016 13:37
%%%-------------------------------------------------------------------
-module(session_storage).
-author("huangcheng").

-behaviour(gen_server).

-include("../include/webrtc.hrl").

-type client() :: {Name :: atom(), Pid :: pid()}.

%%-type status() :: idle | busy.

-type clients() :: [client()].

%% API
-export([start_link/0]).

-export([create_room/2,
         enter_room/2,
         get_server_by_room/1,
         get_client_by_name/2,
         get_client_pid/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE_NAME, sesstion_table).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, ets:new(?ETS_TABLE_NAME, [ordered_set, private])}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}).
handle_call({create, {Room, Server}}, _From, State) ->
    Reply = case ets:match_object(State, #session{room = Room, _ = '_'}) of
                [_] ->
                    {error, ?ERROR_ROOM_ALREADY_EXISTED};
                [] ->
                    RoomInfo = #session{room = Room, server = Server, audiences = []},
                    ets:insert(State, RoomInfo),
                    ok
            end, {reply, Reply, State};
handle_call({enter, {Room, Client}}, _From, State) ->
    {ClientName, _} = Client,
    Reply = case ets:match_object(State, #session{room = Room, _ = '_'}) of
                [Result] ->
                    case get_client_by_name(Result#session.audiences, ClientName) of
                        [_] ->
                            {error, ?ERROR_USER_ALREADY_EXISTED};
                        [] ->
                            NewSession = Result#session{audiences = [Client | Result#session.audiences]},
                            ets:insert(State, NewSession),
                            {ok, Result#session.server}
                    end;
                [] ->
                    {error, ?ERROR_ROOM_DOES_NOT_EXIST}
            end,
    {reply, Reply, State};
handle_call({server, Room}, _From, State) ->
    Reply = case ets:match_object(State, #session{room = Room, _ = '_'}) of
                [Result] ->
                    {ok, Result#session.server};
                [] ->
                    {error, ?ERROR_ROOM_DOES_NOT_EXIST}
            end,
    {reply, Reply, State};
handle_call({client, {Room, ClientName}}, _From, State) ->
    Reply = case ets:match_object(State, #session{room = Room, _ = '_'}) of
                [Result] ->
                    case get_client_by_name(Result#session.audiences, ClientName) of
                        [] ->
                            {error, ?ERROR_CLIENT_DOES_NOT_EXIST};
                        [{_, Pid}] ->
                            {ok, Pid}
                    end;
                [] ->
                    {error, ?ERROR_ROOM_DOES_NOT_EXIST}
            end,
    {reply, Reply, State};
%%handle_call({audiences, Room}, _From, State) ->
%%    Reply = case ets:match_object(State, #session{room = Room, _ = '_'}) of
%%                [Result] ->
%%                    case Audiences = Result#session.audiences of
%%                        [] ->
%%                            {error, ?ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT};
%%                        _ ->
%%                            {ok, Audiences}
%%                    end;
%%                [] ->
%%                    {error, ?ERROR_ROOM_DOES_NOT_EXIST}
%%            end,
%%    {reply, Reply, State};
%%handle_call({idle, Room}, _From, State) ->
%%    Reply = case ?MODULE:get_room_audiences(Room) of
%%                [Result] ->
%%                    case Audiences = Result#session.audiences of
%%                        [] ->
%%                            {error, ?ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT};
%%                        _ ->
%%                            case get_idle_clients(Audiences) of
%%                                [] ->
%%                                    {error, ?ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT};
%%                                Clients ->
%%                                    [Pid || {_, {Pid, _}} <- Clients]
%%                            end
%%                    end;
%%                [] ->
%%                    {error, ?ERROR_ROOM_DOES_NOT_EXIST}
%%            end,
%%    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: term()) -> term()).
terminate(_Reason, _State) ->
    ets:delete(_State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: term(),
    Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a living room.
%%
%% @spec create_room(Room, Server, Anchor) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------

-spec(create_room(Room :: integer(), Server :: pid()) -> ok | {error, Reason :: integer()}).
create_room(Room, Server) ->
    gen_server:call(?SERVER, {create, {Room, Server}}).

-spec(enter_room(Room :: integer(), Client :: client())  -> {ok, Server :: pid()} | {error, Reason :: integer()}).
enter_room(Room, Client) ->
    gen_server:call(?SERVER, {enter, {Room, Client}}).

-spec(get_server_by_room(Room :: integer()) -> {ok, Server :: pid()} | {error, Reason :: integer()}).
get_server_by_room(Room) ->
    gen_server:call(?SERVER, {server, Room}).

-spec(get_client_pid(Room :: integer(), ClientName :: atom()) -> {ok, ClientPid :: pid()} | {error, Reason :: integer()}).
get_client_pid(Room, ClientName) ->
    gen_server:call(?SERVER, {client, {Room, ClientName}}).

-spec(get_client_by_name(Clients :: clients(), Name :: atom()) -> [client()] | []).
get_client_by_name(Clients, Name) ->
    lists:filter(fun(Client) ->
                    {RoomName, _} = Client,
                    Name =:= RoomName
                 end, Clients).
