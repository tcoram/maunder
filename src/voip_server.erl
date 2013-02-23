%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(voip_server).
-author('todd@maplefish.com').
-behaviour(gen_server).
-include("mumble_pb.hrl").

%% External API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {users}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{users=dict:new()},10}.

handle_info(timeout,_S) ->
    {noreply, #state{users=gen_server:call(maunder_server,userstates)}}.

handle_cast({voip, Pid, Msg}, #state{users=U}=State) ->
    <<Type:4,_Target:4, Rest/binary>> = Msg,
    %
    % Apparently, there is no embedded session ID when a voice packet
    % arrives from the client. It only has a sequence number in 'Rest'.
    % So, we don't have to parse the packet to 'replace' the session
    % ID. We can just insert the 'From' session ID.
    %
    % NOTE:
    %  We don't bother removing positional data right now, we just send it all.
    %
    %  Adjust type (removing target) and insert a session ID.
    %
    #userstate{session=From} = dict:fetch(Pid,U),
    Ntype=Type band 16#e0,
    Vfrom = varint:gen(From),
    Fmsg = <<Ntype:8,Vfrom/binary,Rest/binary>>,
    [gen_server:cast(P,{udptunnel, Fmsg}) || 
	P <- dict:fetch_keys(U), P /= Pid],
    {noreply, State};

handle_cast({newuser,Pid,User}, #state{users=U}=State) ->
    NU=dict:store(Pid, User, U),
    {noreply, State#state{users=NU}};
		   
handle_cast({deluser,Pid}, #state{users=U}=State) ->
    NU=dict:erase(Pid, U),
    {noreply, State#state{users=NU}};
		   

handle_cast({userstates,Users}, State) ->
%    io:format("Userstates = ~p~n", [Users]),
    {noreply, State#state{users=Users}}.
    

handle_call(Request, _From, State) ->  {stop, {unknown_call, Request}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

