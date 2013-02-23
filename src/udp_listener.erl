%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(udp_listener).
-author('todd@maplefish.com').
-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {socket,max_users}).

start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

init([Port, _Module]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Port, [binary]),
    gen_udp:controlling_process(Socket, self()),
    {ok, #state{socket=Socket}}.

handle_call(Request, _From, State) ->  {stop, {unknown_call, Request}, State}.

handle_info({udp,S, H, P, <<0,0,0,0,Ident:8/binary>>}, State) ->
    {UC,MC}=gen_server:call(maunder_server,usercount),
    gen_udp:send(S, H, P, <<0,0,0,1,		% version
			    Ident/binary,
			    UC:32/unsigned-big-integer, % user count
			    MC:32/unsigned-big-integer, % max users
			    72000:32/unsigned-big-integer>>), % max bandwidth
    {noreply,State};

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.
handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

