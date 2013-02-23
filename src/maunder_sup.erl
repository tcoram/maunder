%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(maunder_sup).
-author('todd@maplefish.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    MaunderServer = ?CHILD(maunder_server, worker),
    {ok, { {one_for_one, 5, 10}, [MaunderServer]} }.

