-module(mpb).
-author('todd@maplefish.com').

-export([handle_message/3,send_message/4,encode_message/2]).
-include("mumble_pb_ids.hrl").
-include("ssl_client.hrl").


send_message(Type,Msg,SendModule,State) ->
    SendModule:send(encode_message(Type,Msg),State).

handle_message(<< Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,
		  Msg:Len/binary, Rest/binary >>,M,S) ->
    handle_message(Rest,M,handle_pb(Type,Msg,M,S));
handle_message(<<>>,_M,S) -> S;
handle_message(_Blob,_M,S) -> S.		% didn't get everything yet. ERROR?

encode_message(Type, Msg) ->
    BMsg = erlang:iolist_to_binary(Msg),
    Len = byte_size(BMsg),
    << Type:16/unsigned-big-integer, Len:32/unsigned-big-integer,BMsg/binary >>.

handle_pb(?MSG_UDPTUNNEL,Msg,_M,#state{vmrecording=P}=S) ->
    Alive=is_pid(P) andalso is_process_alive(P),
    if Alive ->
	    P ! {data, Msg};
       true ->
	    gen_server:cast(voip_server,
			    {voip, self(), Msg})
    end,
    S;
	
handle_pb(?MSG_VERSION,_Msg,M,S) ->
    R=mumble_pb:encode_version(gen_server:call(maunder_server,version)),
    M:send(encode_message(?MSG_VERSION,R),S),
    S;

handle_pb(?MSG_PING,Msg,M,S) ->
    M:send(encode_message(?MSG_PING,Msg),S),
    S;

handle_pb(?MSG_AUTHENTICATE,Msg,M,S) ->
    {_,U,P,_T,_C,_O} = mumble_pb:decode_authenticate(Msg),
    Sid=gen_server:call(maunder_server,{authenticate, U,P}),
    CS=gen_server:call(maunder_server,channelstates),
    lists:foreach(fun(K) -> 
			  R=mumble_pb:encode_channelstate(dict:fetch(K,CS)),
			  M:send(encode_message(?MSG_CHANNELSTATE,R),S)
		  end, dict:fetch_keys(CS)),
    US=gen_server:call(maunder_server,userstates),
    lists:foreach(fun(K) -> 
			  R=mumble_pb:encode_userstate(dict:fetch(K,US)),
			  M:send(encode_message(?MSG_USERSTATE,R),S)
		  end, dict:fetch_keys(US)),
    K = mumble_pb:encode_cryptsetup(gen_server:call(maunder_server,cryptsetup)),
    M:send(encode_message(?MSG_CRYPTSETUP, K),S),

    V=mumble_pb:encode_codecversion(gen_server:call(maunder_server,codecversion)),
    M:send(encode_message(?MSG_CODECVERSION, V),S),

    C=mumble_pb:encode_serverconfig(gen_server:call(maunder_server,serverconfig)),
    M:send(encode_message(?MSG_SERVERCONFIG,C),S),

    R=mumble_pb:encode_serversync(gen_server:call(maunder_server,
						  {serversync,Sid})),
    M:send(encode_message(?MSG_SERVERSYNC,R),S),
    S#state{id=Sid};

handle_pb(?MSG_TEXTMESSAGE,Msg,_M,S) ->
    T=mumble_pb:decode_textmessage(Msg),
    io:format("Msg = ~p~n", [T]),
    case T of
	{_,_,[],[0],_,Txt} ->
	    Pre=lists:sublist(Txt,4),
	    if Pre == "#vm:" ->
		    Ns=ssl_client:start_vrecorder(S),
		    Ns#state.vmrecording ! start,
		    rootmsg(<<"Recording... end with #discard or #save">>),
		    Ns;
	       Pre == "#pla" ->
		    rootmsg(<<"Playback.">>),
		    S#state.vmrecording ! {playback,self()},
		    S;
	       Pre == "#sto" ->
		    S#state.vmrecording ! stop,
		    io:format("Stop Voicemail!~n"),
		    rootmsg(<<"Finished Recording.">>),
		    S#state{vmrecording=false};
	       true ->
		    gen_server:cast(maunder_server,{textmessage,self(),T}),
		    S
	    end;
	_ ->
	    gen_server:cast(maunder_server,{textmessage,self(),T}),
	    S
    end;

handle_pb(?MSG_PERMISSIONQUERY,Msg,M,S) ->
    P=mumble_pb:decode_permissionquery(Msg),
    R=mumble_pb:encode_permissionquery(gen_server:call(maunder_server,
						       {permissionquery,P})),
    M:send(encode_message(?MSG_PERMISSIONQUERY,R),S), 
    S;

handle_pb(?MSG_USERSTATE,Msg,_M,S) ->
    U=mumble_pb:decode_userstate(Msg),
    gen_server:cast(maunder_server,{moduserstate,self(),U}),
    S;

handle_pb(Any, Msg,_M,S) ->
    io:format("Unhandled ~p <~p>~n", [Any,Msg]),
    S.

rootmsg(Msg) ->
    Reply={textmessage,0,[],[],[],Msg},
    gen_server:cast(self(),{textmessage,Reply}).

