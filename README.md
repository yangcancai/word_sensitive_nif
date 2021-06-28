# word_sensitive_nif
A library for sensitive string matching, the implementation language is rust/erlang, and the algorithm used is ac

![CI](https://github.com/yangcancai/word_sensitive_nif/actions/workflows/ci.yml/badge.svg)

# How to use?

* 1. New a Reference
* 2. Add keywords
* 3. Build
* 4. Query Some Things
* 5. Weight of Category for Keywords

## Dependencies

```erlang
{deps, 
[
	{word_sensitive_nif,{git, "https://github.com/yangcancai/word_sensitive_nif.git", {branch,"main"}}}
]
}.
```
## Clone to Develop 

```erlang
$ git clone https://github.com/yangcancai/word_sensitive_nif.git
$ cd word_sensitive_nif
$ make shell
# ./tool.sh replace_config
./rebar3 as test shell
===> Verifying dependencies...
   Compiling word_sensitive_nif v0.1.0 (/Users/admin/proj/erlang/word_sensitive_nif/crates/word_sensitive)
    Finished release [optimized] target(s) in 1.83s
===> Analyzing applications...
===> Compiling word_sensitive_nif
Erlang/OTP 22 [erts-10.7.2.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.7.2.1  (abort with ^G)
1> ===> Booted word_sensitive_nif
1> {ok, Ref} = word_sensitive:new().
{ok,#Ref<0.1333260717.290586629.145298>}
2> word_sensitive:add_key_word(Ref, <<"abc">>).
ok
3> word_sensitive:build(Ref).
ok
4> Result = word_sensitive:query(Ref, <<"abc">>).
[<<"abc">>]
5> word_sensitive:query_total_weight(Ref, <<"abc">>).
1
6> word_sensitive:query_cate_weight(Ref, <<"abc">>).
#{1 => 1}
7> 
```

# Common Test

```erlang
   {ok, Ref} = word_sensitive:new(),
    ok = word_sensitive:add_key_word(Ref, <<"abc">>),
    word_sensitive:build(Ref),
    Result = word_sensitive:query(Ref, <<"abc">>),
    ?assertEqual([<<"abc">>], Result),
    ?assertEqual(1, word_sensitive:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1}, word_sensitive:query_cate_weight(Ref, <<"abc">>)),

    word_sensitive:add_key_word(Ref,
                                        <<"bc">>,
                                        2,
                                        3),
    word_sensitive:add_key_word(Ref,
                                        <<"cd">>,
                                        1,
                                        3),

    word_sensitive:build(Ref),
    ?assertEqual(4, word_sensitive:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1, 2 => 3}, word_sensitive:query_cate_weight(Ref, <<"abc">>)),
    ?assertEqual(7, word_sensitive:query_total_weight(Ref, <<"abcd">>)),
    ?assertEqual(#{1 => 4, 2 => 3}, word_sensitive:query_cate_weight(Ref, <<"abcd">>)),

    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive:query(Ref, <<"abcd">>)),

    ok = word_sensitive:add_key_word(Ref, <<"历史"/utf8>>),
    ok = word_sensitive:add_key_word(Ref, <<"物理"/utf8>>),
    word_sensitive:build(Ref),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive:query(Ref, <<"abcd">>)),
    ?assertEqual([<<"历史"/utf8>>], word_sensitive:query(Ref, <<"我要上历史课"/utf8>>)),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"历史"/utf8>>],
                 word_sensitive:query(Ref, <<"abc我要上历史课"/utf8>>)),


   {ok, Ref1} = word_sensitive:new(),
    ok = word_sensitive:add_key_word(Ref1, <<"abc">>),
    word_sensitive:build(Ref1),
    ?assertEqual([<<"abc">>], word_sensitive:query(Ref1, <<"abcd">>)),
    ok = word_sensitive:add_key_word(Ref1, <<"bc">>, 10),
    ok = word_sensitive:add_key_word(Ref1, <<"ce">>, 2, 20),
    word_sensitive:build(Ref1),
    ?assertEqual(#{1 => 11, 2 => 20}, word_sensitive:query_cate_weight(Ref1, <<"abce">>)),
    ?assertEqual(31, word_sensitive:query_total_weight(Ref1, <<"abce">>)),

    ?assertEqual({31, #{1 => {11, [<<"abc">>, <<"bc">>]}, 2 => {20, [<<"ce">>]}}},
                 word_sensitive:query_all(Ref1, <<"abce">>)),

```

# Reference

[word_sensitive](https://github.com/yangcancai/word_sensitive)