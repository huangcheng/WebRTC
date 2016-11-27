%%%-------------------------------------------------------------------
%%% @author huangcheng
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2016 13:51
%%%-------------------------------------------------------------------
-author("huangcheng").

-define(SESSION_STORAGE, session_storage).


%%%-------------------------------------------------------------------
%%% Error codes definitions.
%%%-------------------------------------------------------------------
-define(ERROR_ROOM_ALREADY_EXISTED, -100).
-define(ERROR_USER_ALREADY_EXISTED, -101).
-define(ERROR_ROOM_DOES_NOT_EXIST, -102).
-define(ERROR_CLIENT_DOES_NOT_EXIST, -103).
-define(ERROR_ROOM_DOES_NOT_HAVE_ANY_CLIENT, -104).


%%%-------------------------------------------------------------------
%%% Status codes definitions.
%%%-------------------------------------------------------------------
-define(STATUS_UNSUCCESSFUL, -1).
-define(STATUS_UNSUPPORTED_OPERATION, 0).
-define(STATUS_ROOM_CREATED, 1).
-define(STATUS_ROOM_ENTERED, 2).
-define(STATUS_OFFER_SENT, 3).
-define(STATUS_ANSWER_SENT, 4).
-define(STATUS_SERVER_CANDIDATE_SENT, 5).
-define(STATUS_CLIENT_CANDIDATE_SENT, 6).


%%%-------------------------------------------------------------------
%%% Event codes definitions.
%%%-------------------------------------------------------------------
-define(EVENT_CLIENT_ENTER, 100).
-define(EVENT_OFFER_RECEIVED, 101).
-define(EVENT_ANSWER_RECEIVED, 102).
-define(EVENT_SERVER_CANDIDATE_RECEIVED, 103).
-define(EVENT_CLIENT_CANDIDATE_RECEIVED, 104).



-record(session, {room :: integer(),
                  server :: pid(),
                  audiences ::[pid()]}).