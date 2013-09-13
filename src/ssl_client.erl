%%%-------------------------------------------------------------------
%% The contents of this file are subject to the Mozilla Public License
%% Version 2.0 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(ssl_client).

-behaviour(gen_server).
-define(MAX_IDLE_MS,30000).
-include("mumble_pb_ids.hrl").
-include("mumble_pb.hrl").
-include("ssl_client.hrl").

%% API
-export([start_link/0,send/2,vrecord/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_vrecorder/1]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{},?MAX_IDLE_MS}.

start_vrecorder(#state{vmrecording=P}=S) ->
    io:format("start recorder~n"),
    Alive=is_pid(P) andalso is_process_alive(P),
    if Alive ->
	    S;
       true ->
	    Vpid = spawn_link(?MODULE,vrecord,[S#state.id,[],erlang:now()]),
	    io:format("Vpid=~p~n", [Vpid]),
	    S#state{vmrecording=Vpid}
    end.

vrecord(From,Accum,Last_now) ->
    receive
	start -> 
	    io:format("Starting!~n"),
	    vrecord(From,[],erlang:now());
	{data, Data} -> 
	    Now = erlang:now(),
	    <<Type:4,_Target:4, Rest/binary>> = Data,
	    Ntype=Type band 16#e0,
	    Vfrom = varint:gen(0),		%session 0 is always Root
	    Fmsg = <<Ntype:8,Vfrom/binary,Rest/binary>>,
	    vrecord(From,Accum ++ [{(timer:now_diff(Now,Last_now) div 1000)+1,Fmsg}],Now);
	{playback,Pid} ->
	    io:format("Message packet count=~p~n", [length(Accum)]),
	    vplayback(Pid,Accum),
	    vrecord(From,Accum,Last_now);
	discard -> io:format("Discard~n"), exit(normal);
	save -> io:format("Save~n"),
		exit(normal);
	_Msg -> io:format("Ack: ~p~n", [_Msg]),
		exit(normal)
%    after 60000 -> io:format("Timout~n")
    end.

vplayback(_Parent,[]) -> [];
vplayback(Parent,[{Wait,Msg}|Rest]) ->
    timer:sleep(Wait),
    gen_server:cast(Parent,{udptunnel, Msg}),
%    io:format("Msg ~p ~n",[Msg]),
    vplayback(Parent,Rest).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({socket,Socket}, _State) ->
    ssl:controlling_process(Socket, self()),
    ssl:ssl_accept(Socket),
    handle_incoming(#state{socket=Socket,vmrecording=false}),
    {noreply, #state{socket=Socket,vmrecording=false}};

%    ssl:setopts(Socket, [{active, false}]),
%    io:format("Connected~n"),

handle_cast({udptunnel,Data},State) ->
    io:format("udptunnel?~n"),
    send(mpb:encode_message(?MSG_UDPTUNNEL,Data),State),
    {noreply,State};

handle_cast({textmessage,Data},State) ->
    io:format("Text message: ~p~n",[Data]),
    T=mumble_pb:encode_textmessage(Data),
    send(mpb:encode_message(?MSG_TEXTMESSAGE,T),State),
    {noreply,State};

handle_cast({userstate,Data},State) ->
    io:format("New user state = ~p~n", [Data]),
    U=mumble_pb:encode_userstate(Data),
    send(mpb:encode_message(?MSG_USERSTATE,U),State),
    {noreply,State};

handle_cast({newuser,UR},State) ->
    io:format("New user: ~p~n",[UR]),
    R=mumble_pb:encode_userstate(UR),
    mpb:send_message(?MSG_USERSTATE,R,?MODULE,State),
    {noreply,State};

handle_cast({deluser,UR},State) ->
    io:format("Deleting user: ~p~n",[UR]),
    R=mumble_pb:encode_userremove(UR),
    mpb:send_message(?MSG_USERREMOVE,R,?MODULE,State),
    {noreply,State};

handle_cast(_Msg, State) ->
    io:format("Cast?~n"),
    {noreply, State}.

get_type(Sock) ->
    io:format("Get type..."),
  case ssl:recv(Sock, 2) of
    {ok, << T:16/unsigned-big-integer >>} ->  T;
    {error, Reason} ->
      exit(Reason)
   end.

get_len(Sock) ->
    io:format("~nGet len..."),
  case ssl:recv(Sock, 4) of
    {ok, << Len:32/unsigned-big-integer >>} ->  Len;
    {error, Reason} ->
      exit(Reason)
   end.

get_payload(Sock,Len) ->
    io:format("~nGet payload..."),
  case ssl:recv(Sock, Len) of
    {ok, Pay} ->  Pay;
    {error, Reason} ->
      exit(Reason)
   end.
    
handle_incoming(#state{socket=Sock}=State) ->
    T=get_type(Sock),
    L=get_len(Sock),
    Pay=get_payload(Sock,L),
    io:format("Incoming->~p/~p/~p~n",[T,L,Pay]),
    Nstate=mpb:handle_message(T,Pay,?MODULE,State),
    handle_incoming(Nstate).

handle_info(timeout,State) ->
    io:format("Timeout! ~p seconds idle~n", [?MAX_IDLE_MS/1000]),
    {stop, normal, State};
handle_info({ssl_closed, _S}, State) ->
    io:format("Disconnected!~n"),
    gen_server:call(maunder_server,deluser),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(Data,#state{socket=Socket}=_State) ->
    ssl:send(Socket, Data).
