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
%%% Created : 2021-06-11T08:22:13+00:00
%%%-------------------------------------------------------------------
-module(word_sensitive_SUITE).

-author("yangcancai").

-include("word_sensitive_ct.hrl").

-compile(export_all).

-define(APP, word_sensitive_nif).

all() ->
    [query_total_weight, clear, word_sensitive].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(?APP),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    ok = application:stop(?APP),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    % ok = meck:new(word_sensitive, [non_strict, no_link]),
    ok.

expect() ->
    % ok = meck:expect(word_sensitive, test, fun() -> {ok, 1} end).
    ok.

del_meck() ->
    meck:unload().

query_total_weight(_) ->
    {ok, Ref} = word_sensitive_nif:new(),
    ok = word_sensitive_nif:add_key_word(Ref, <<"abc">>),
    word_sensitive_nif:build(Ref),
    Result = word_sensitive_nif:query(Ref, <<"abc">>),
    ?assertEqual([<<"abc">>], Result),
    ?assertEqual(1, word_sensitive_nif:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1}, word_sensitive_nif:query_cate_weight(Ref, <<"abc">>)),

    word_sensitive_nif:add_key_word_ext(Ref,
                                        <<"bc">>,
                                        #ext{cate = #{2 => 3, 4 => 10},
                                             len = 2}),
    word_sensitive_nif:add_key_word_ext(Ref,
                                        <<"cd">>,
                                        #ext{cate = #{1 => 3},
                                             len = 2
                                             }),

    word_sensitive_nif:build(Ref),

    ?assertEqual({14,#{1 => {1,[<<"abc">>]},2 => {3,[<<"bc">>]}, 4 => {10, [<<"bc">>]}}}, word_sensitive_nif:query_all(Ref, <<"abc">>)),
    ?assertEqual(14, word_sensitive_nif:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1, 2 => 3, 4 => 10}, word_sensitive_nif:query_cate_weight(Ref, <<"abc">>)),
    ?assertEqual(17, word_sensitive_nif:query_total_weight(Ref, <<"abcd">>)),
    ?assertEqual(#{1 => 4, 2 => 3, 4 => 10}, word_sensitive_nif:query_cate_weight(Ref, <<"abcd">>)),

    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive_nif:query(Ref, <<"abcd">>)),

    ok = word_sensitive_nif:add_key_word(Ref, <<"历史"/utf8>>),
    ok = word_sensitive_nif:add_key_word(Ref, <<"物理"/utf8>>),
    word_sensitive_nif:build(Ref),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive_nif:query(Ref, <<"abcd">>)),
    ?assertEqual([<<"历史"/utf8>>], word_sensitive_nif:query(Ref, <<"我要上历史课"/utf8>>)),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"历史"/utf8>>],
                 word_sensitive_nif:query(Ref, <<"abc我要上历史课"/utf8>>)),
    ok.

clear(_) ->
    {ok, Ref} = word_sensitive_nif:new(),
    ok = word_sensitive_nif:add_key_word(Ref, <<"abc">>),
    word_sensitive_nif:build(Ref),
    Result = word_sensitive_nif:query(Ref, <<"abc">>),
    ?assertEqual([<<"abc">>], Result),
    ok = word_sensitive_nif:clear(Ref),
    ?assertEqual([], word_sensitive_nif:query(Ref, <<"abc">>)),
    ok.

word_sensitive(_) ->
    {ok, Ref} = word_sensitive:new(),
    ok = word_sensitive:add_key_word(Ref, <<"abc">>),
    word_sensitive:build(Ref),
    ?assertEqual([<<"abc">>], word_sensitive:query(Ref, <<"abcd">>)),
    ok = word_sensitive:add_key_word(Ref, <<"bc">>, 10),
    ok = word_sensitive:add_key_word(Ref, <<"ce">>, 2, 20),
    word_sensitive:build(Ref),
    ?assertEqual(#{1 => 11, 2 => 20}, word_sensitive:query_cate_weight(Ref, <<"abce">>)),
    ?assertEqual(31, word_sensitive:query_total_weight(Ref, <<"abce">>)),

    ?assertEqual({31, #{1 => {11, [<<"abc">>, <<"bc">>]}, 2 => {20, [<<"ce">>]}}},
                 word_sensitive:query_all(Ref, <<"abce">>)),
    ok.
