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
-module(word_sensitive).

-include("word_sensitive_nif.hrl").

-author("yangcancai").

-export([new/0, new/1, add_key_word/2, add_key_word/3, add_key_word/4, build/1, query/2,
         query_total_weight/2, query_cate_weight/2, clear/1]).

-dialyzer({[nowarn_function],
           [add_key_word/2,
            add_key_word/3,
            add_key_word/4,
            build/1,
            query/2,
            query_cate_weight/2,
            query_total_weight/2,
            clear/1]}).

new() ->
    word_sensitive_nif:new().

new(Opt) ->
    word_sensitive_nif:new(Opt).

add_key_word(Ref, Keyword) when is_reference(Ref), is_binary(Keyword) ->
    word_sensitive_nif:add_key_word(Ref, Keyword).

add_key_word(Ref, Keyword, Weight)
    when is_reference(Ref), is_integer(Weight), is_binary(Keyword) ->
    add_key_word(Ref, Keyword, #ext{weight = Weight});
add_key_word(Ref, Keyword, #ext{} = Ext) when is_reference(Ref), is_binary(Keyword) ->
    word_sensitive_nif:add_key_word_ext(Ref, Keyword, Ext#ext{len = erlang:size(Keyword)}).

add_key_word(Ref, Keyword, Cate, Weight)
    when is_reference(Ref), is_integer(Weight), is_integer(Cate) ->
    add_key_word(Ref, Keyword, #ext{weight = Weight, cate = Cate}).

build(Ref) when is_reference(Ref) ->
    word_sensitive_nif:build(Ref).

query(Ref, Text) when is_reference(Ref), is_binary(Text) ->
    word_sensitive_nif:query(Ref, Text).

query_total_weight(Ref, Text) when is_reference(Ref), is_binary(Text) ->
    word_sensitive_nif:query_total_weight(Ref, Text).

query_cate_weight(Ref, Text) when is_reference(Ref), is_binary(Text) ->
    word_sensitive_nif:query_cate_weight(Ref, Text).

clear(Ref) when is_reference(Ref) ->
    word_sensitive_nif:clear(Ref).
