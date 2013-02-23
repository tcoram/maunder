%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(maunder_app).
-author('todd@maplefish.com').

% Design/Layout heavily based on 
% http://trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles

-behaviour(application).

-export([start_ssl_client/0]).

%% Application callbacks
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(DEF_PORT,    2222).

-define(SSL_OPTIONS, [binary, {nodelay, true}, {active, false}, {reuseaddr, true}, {certfile, "server.pem"}, {versions, [tlsv1]}]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start_ssl_client() ->
    supervisor:start_child(ssl_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, ssl_client]).

stop(_S) ->
    ok.
%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module]) ->
    io:format("init(~p,~p])~n",[Port,Module]),
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
						% SSL Listener
       {   ssl_server_sup,		      % Id       = internal id
	   {ssl_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
	   permanent, % Restart  = permanent | transient | temporary
	   2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,    % Type     = worker | supervisor
	   [ssl_listener]	       % Modules  = [Module] | dynamic
       },
       {   udp_server_sup,		      % Id       = internal id
	   {udp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
	   permanent, % Restart  = permanent | transient | temporary
	   2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,    % Type     = worker | supervisor
	   [udp_listener]	       % Modules  = [Module] | dynamic
       },
       {   maunder_server_sup,		      % Id       = internal id
	   {maunder_server,start_link,[]}, % StartFun = {M, F, A}
	   permanent, % Restart  = permanent | transient | temporary
	   2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,    % Type     = worker | supervisor
	   [maunder_server]	       % Modules  = [Module] | dynamic
       },
       {   voip_server_sup,		      % Id       = internal id
	   {voip_server,start_link,[]}, % StartFun = {M, F, A}
	   permanent, % Restart  = permanent | transient | temporary
	   2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,    % Type     = worker | supervisor
	   [voip_server]	       % Modules  = [Module] | dynamic
       },
       {   ssl_client_sup,
	   {supervisor,start_link,[{local, ssl_client_sup}, ?MODULE, [Module]]},
	   permanent, % Restart  = permanent | transient | temporary
	   infinity,  % Shutdown = brutal_kill | int() >= 0 | infinity
	   supervisor,		      % Type     = worker | supervisor
	   []			      % Modules  = [Module] | dynamic
       }
      ]
     }
    };

init([Module]) ->
    io:format("init(~p)~n",[Module]),
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
						% SSL Client
       {   undefined,			      % Id       = internal id
	   {Module,start_link,[]},	      % StartFun = {M, F, A}
	   temporary, % Restart  = permanent | transient | temporary
	   2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
	   worker,    % Type     = worker | supervisor
	   []	      % Modules  = [Module] | dynamic
       }
      ]
     }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
