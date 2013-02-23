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
-module(ssl_listener).
-author('todd@maplefish.com').

-behaviour(gen_server).

-define(SSL_OPTIONS, [binary, {nodelay, true}, {active, false}, {reuseaddr, true}, {certfile, "server.pem"}, {versions, [tlsv1]}]).


%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
	  listener,       % Listening socket
	  server	  % server pid
	 }).

start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).

init([Port, _Module]) ->
    process_flag(trap_exit, true),
    case ssl:listen(Port, ?SSL_OPTIONS) of
	{ok, Listen_socket} ->
	    gen_server:cast(self(), {accepted, self()}),
	{ok, #state{listener = Listen_socket}};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State = #state{}) ->
	{noreply, wait_for_new_client(State)}.

wait_for_new_client(#state{listener = Socket, 
			   server=ServerPid} = State) ->
    io:format("Waiting for connections...~n"),
    {ok, SslSocket} = ssl:transport_accept(Socket),
    gen_server:cast(self(), {accepted, self()}),
    {ok, Pid} = maunder_app:start_ssl_client(),
    gen_server:cast(Pid, {socket, SslSocket}),
    State.

handle_call(Request, _From, State) ->  {stop, {unknown_call, Request}, State}.
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, State) ->
    ssl:close(State#state.listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

