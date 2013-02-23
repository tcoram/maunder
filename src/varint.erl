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
-module(varint).
-export([gen/1]).

gen(Num) when (Num < 16#80) ->
    <<Num:8>>;
gen(Num) when (Num < 16#4000) ->
    Val1 = (Num bsr 8) bor 16#80,
    Val2 = Num band 16#ff,
    <<Val1:8,Val2:8>>;
gen(Num) when (Num < 16#200000) ->
    Val1 = (Num bsr 16) bor 16#c0,
    Val2 = (Num bsr 8) band 16#ff,
    Val3 = Num band 16#ff,
    <<Val1:8,Val2:8,Val3:8>>;
gen(Num) when (Num < 16#10000000) ->
    Val1 = (Num bsr 24) bor 16#e0,
    Val2 = (Num bsr 16) band 16#ff,
    Val3 = (Num bsr 8) band 16#ff,
    Val4 = Num band 16#ff,
    <<Val1:8,Val2:8,Val3:8,Val4:8>>;
gen(Num) when (Num < 16#100000000) ->
    Val1 = 16#f0,
    Val2 = (Num bsr 24) bor 16#ff,
    Val3 = (Num bsr 16) band 16#ff,
    Val4 = (Num bsr 8) band 16#ff,
    Val5 = Num band 16#ff,
    <<Val1:8,Val2:8,Val3:8,Val4:8,Val5:8>>;
gen(Num) ->
    Val1 = 16#f4,
    Val2 = (Num bsr 56) bor 16#ff,
    Val3 = (Num bsr 48) band 16#ff,
    Val4 = (Num bsr 40) band 16#ff,
    Val5 = (Num bsr 32) band 16#ff,
    Val6 = (Num bsr 24) band 16#ff,
    Val7 = (Num bsr 16) band 16#ff,
    Val8 = (Num bsr 8) band 16#ff,
    Val9 = Num band 16#ff,
    <<Val1:8,Val2:8,Val3:8,Val4:8,Val4:8,Val5:8,Val6:8,Val7:8,Val8:8,Val9:8>>.
    


