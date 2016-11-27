%%%-------------------------------------------------------------------
%%% @author HuangCheng
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Nov 2016 16:53
%%%-------------------------------------------------------------------
-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-author("HuangCheng").

-include("../include/webrtc.hrl").

%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->

    CodeMessageMappings = #{
        ?ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT => <<"Error: Room does not have any client.">>,
        ?ERROR_ROOM_ALREADY_EXISTED => <<"Error: Room already existed.">>,
        ?ERROR_CLIENT_DOES_NOT_EXIST => <<"Error: Client does not existed.">>,
        ?ERROR_USER_ALREADY_EXISTED => <<"Error: User already existed.">>,
        ?ERROR_ROOM_DOES_NOT_EXIST => <<"Error: Room does not exist.">>
    },
    Request = [{list_to_atom(binary_to_list(Key)), Val} || {Key, Val} <- jsx:decode(Msg)],
    [{action, Action} | T] = Request,
    Reply = case list_to_atom(binary_to_list(Action)) of
                create ->
                    [{room, Room}] = T,
                    case ?SESSION_STORAGE:create_room(Room, self()) of
                        ok ->
                            jsx:encode([{code, ?STATUS_ROOM_CREATED}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                enter ->
                    [{room, Room}, {client, Name}] = T,
                    case ?SESSION_STORAGE:enter_room(Room, {list_to_atom(binary_to_list(Name)), self()}) of
                        {ok, Server} ->
                            Server ! {enter, jsx:encode([{code, ?EVENT_CLIENT_ENTER}, {message, Name}])},
                            jsx:encode([{code, ?STATUS_ROOM_ENTERED}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                offer ->
                    [{room, Room}, {client, Name}, {sdp, Sdp}] = T,
                    case ?SESSION_STORAGE:get_client_pid(Room, list_to_atom(binary_to_list(Name))) of
                        {ok, Client} ->
                            Client ! {offer, jsx:encode([{code, ?EVENT_OFFER_RECEIVED}, {message, Sdp}])},
                            jsx:encode([{code, ?STATUS_OFFER_SENT}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                answer ->
                    [{room, Room}, {client, Client}, {sdp, Sdp}] = T,
                    case ?SESSION_STORAGE:get_server_by_room(Room) of
                        {ok, Server} ->
                            Server ! {answer, jsx:encode([{code, ?EVENT_ANSWER_RECEIVED}, {message, [{client, Client}, {sdp, Sdp}]}])},
                            jsx:encode([{code, ?STATUS_ANSWER_SENT}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                server_ice ->
                    [{room, Room}, {client, Client}, {candidate, ServerICE}] = T,
                    case ?SESSION_STORAGE:get_client_pid(Room, list_to_atom(binary_to_list(Client))) of
                        {ok, ClientPid} ->
                            ClientPid ! {server_ice, jsx:encode([{code, ?EVENT_SERVER_CANDIDATE_RECEIVED}, {message, ServerICE}])},
                            jsx:encode([{code, ?STATUS_SERVER_CANDIDATE_SENT}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                client_ice ->
                    [{room, Room}, {client, Client}, {candidate, ClientICE}] = T,
                    case ?SESSION_STORAGE:get_server_by_room(Room) of
                        {ok, Server} ->
                            Server ! {client_ice, jsx:encode([{code, ?EVENT_CLIENT_CANDIDATE_RECEIVED}, {message, [{client, Client}, {candidate, ClientICE}]}])},
                            jsx:encode([{code, ?STATUS_CLIENT_CANDIDATE_SENT}, {message, <<"">>}]);
                        {error, Reason} ->
                            jsx:encode([{code, ?STATUS_UNSUCCESSFUL}, {message, maps:get(Reason, CodeMessageMappings)}])
                    end;
                _ ->
                    jsx:encode([{code, ?STATUS_UNSUPPORTED_OPERATION}, {message, << "">>}])
            end,
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({_, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
