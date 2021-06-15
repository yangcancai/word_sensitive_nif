%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%%% @doc
%%%
%%% @end
%%% Created : 2021-06-15T06:42:19+00:00
%%%-------------------------------------------------------------------
-module(word_sensitive_nif).

%% API
-export([new/0, new/1, add_key_word/2, add_key_word_ext/3, build/1, query/2,
         query_total_weight/2, query_cate_weight/2, clear/1]).

                     %% new resource

            %% clear resource

%% Native library support
-export([load/0]).

-on_load load/0.

-type key_words() :: [binary()].

-opaque word_sensitive() :: reference().

-include("word_sensitive_nif.hrl").

-export_type([word_sensitive/0]).

new() ->
    new(#{}).

-spec new(_Opts :: map()) -> {ok, Ref :: word_sensitive()} | {error, Reason :: binary()}.
new(_Opts) ->
    not_loaded(?LINE).

-spec add_key_word(Ref :: word_sensitive(), Keywords :: key_words()) -> ok.
add_key_word(_Ref, _Keywords) ->
    not_loaded(?LINE).

-spec add_key_word_ext(Ref :: word_sensitive(), Keywords :: key_words(), Ext :: #ext{}) ->
                          ok.
add_key_word_ext(_Ref, _Keywords, _Ext) ->
    not_loaded(?LINE).

-spec build(Ref :: word_sensitive()) -> ok.
build(_Ref) ->
    not_loaded(?LINE).

-spec query(Ref :: word_sensitive(), Text :: binary()) -> ok.
query(_Ref, _Text) ->
    not_loaded(?LINE).

-spec query_total_weight(Ref :: word_sensitive(), Text :: binary()) -> integer().
query_total_weight(_Ref, _Text) ->
    not_loaded(?LINE).

query_cate_weight(_Ref, _Text) ->
    not_loaded(?LINE).

-spec clear(Ref :: word_sensitive()) -> ok.
clear(_Ref) ->
    not_loaded(?LINE).

%% @private
load() ->
    erlang:load_nif(
        filename:join(priv(), "libword_sensitive"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir =
                filename:dirname(
                    code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
