%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(maunder_server).
-author('todd@maplefish.com').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-include("mumble_pb.hrl").


% -record(session, {userstate, cryptkeys, udpenabled}).
% -record(state, {sessions, channels, lastsid}).
-record(state, {users,channels,cryptkeys,lastsid}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    C=dict:store(0, #channelstate{ channel_id=0,parent=0, name= <<"Root">>},
		 dict:new()),
    {ok, #state{users=dict:new(), channels=C, cryptkeys=dict:new(), lastsid=0}}.

handle_call(version, {_Pid,_}, S) ->
    {reply, #version{version=1, release= <<"maunder">>, os= <<"linux">>, 
		     os_version= <<"unk">>}, S};
handle_call(codecversion, {_Pid,_}, S) ->
    {reply, #codecversion{alpha=-2147483637, beta=0, prefer_alpha=true, 
			  opus=false}, S};

handle_call({permissionquery,Perm}, {_Pid,_}, S) ->
    {reply, Perm#permissionquery{permissions=16#f07ff}, S};

handle_call(cryptsetup, {Pid,_}, #state{cryptkeys=C}=State) ->
    Cs = #cryptsetup{key=crypto:rand_bytes(16), 
		     client_nonce=crypto:rand_bytes(16), 
		     server_nonce=crypto:rand_bytes(16)},
    {reply, Cs , State#state{cryptkeys=dict:store(Pid,Cs,C)}};

handle_call({authenticate,User,_Pass}, {Pid,_}, 
	    #state{users=U,lastsid=L}=State) ->
    UR=#userstate{name=User,user_id=L+1, session=L+1},
    NU=dict:store(Pid, UR, U),
    S=State#state{lastsid=L+1, users=NU},
%    gen_server:cast(voip_server,{userstates,NU}), % give voip_server user list
    gen_server:cast(voip_server,{newuser,Pid,UR}), % update voip_server user list
    [gen_server:cast(P,{newuser, UR}) || P <- dict:fetch_keys(NU)],
    {reply, UR#userstate.session, S};

handle_call(deluser,{Pid,_}, #state{users=U}=State) ->
    US=dict:fetch(Pid, U),
    UR=#userremove{ session=US#userstate.session, reason= <<"Gone Away">>},
    NU=dict:erase(Pid,U),
    gen_server:cast(voip_server,{deluser,Pid}), % update voip_server user list
    [gen_server:cast(P,{deluser, UR}) || P <- dict:fetch_keys(NU)],
    {reply, ok, State#state{users=NU}};

handle_call(usercount, _From, State) ->
    {reply, {dict:size(State#state.users),10}, State};

handle_call(channelstates, {_Pid,_}, #state{channels=C}=State) ->
    {reply, C, State};

handle_call(userstates, {_Pid,_}, #state{users=U}=State) ->
    {reply, U, State};

handle_call({serversync,Session}, {_Pid,_}, State) ->
    {reply, {serversync,Session,240000,<<"Welcome to Hell.">>,undefined}, State};

handle_call(serverconfig, {_Pid,_}, State) ->
    {reply, {serverconfig,240000,undefined,true,128,undefined}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({moduserstate, Pid, Ustate},  #state{users=U}=State) ->
    #userstate{session=From} = dict:fetch(Pid,U),
    NUstate = Ustate#userstate{actor=From,session=From},
    [gen_server:cast(P,{userstate,NUstate}) || 
	P <- dict:fetch_keys(U)],
    {noreply, State};
    
handle_cast({textmessage, Pid, Msg},  #state{users=U}=State) ->
    #userstate{session=From} = dict:fetch(Pid,U),
    NMsg = Msg#textmessage{actor=From},
    [gen_server:cast(P,{textmessage, NMsg}) || 
	P <- dict:fetch_keys(U), P /= Pid],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
