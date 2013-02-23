%%%-------------------------------------------------------------------
%%% @author Todd Coram <todd@maplefish.com>
%%% @copyright (C) 2012,2013 Todd Coram
%%% @doc
%%%
%%% @end
%%% Created : 27 Jul 2012 by Todd Coram <todd@maplefish.com)>
%%%-------------------------------------------------------------------
-module(maunder).

-export([start/0, stop/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

%% @spec start() -> ok
%% @doc Start the maunder server.
start() ->
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    application:start(maunder).

%% @spec stop() -> ok
%% @doc Stop the maunder server.
stop() ->
	application:stop(maunder).
