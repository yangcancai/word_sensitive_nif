# word_sensitive_nif
A library for sensitive string matching, the implementation language is rust/erlang, and the algorithm used is ac

![CI](https://github.com/yangcancai/word_sensitive_nif/actions/workflows/ci.yml/badge.svg)

# How to use?

* 1. New a Reference
* 2. Add keywords
* 3. Build
* 4. Query Some Things

```erlang
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
1> {ok, Ref} = word_sensitive_nif:new().
{ok,#Ref<0.1333260717.290586629.145298>}
2> word_sensitive_nif:add_key_word(Ref, <<"abc">>).
ok
3> word_sensitive_nif:build(Ref).
ok
4> Result = word_sensitive_nif:query(Ref, <<"abc">>).
[<<"abc">>]
5> word_sensitive_nif:query_total_weight(Ref, <<"abc">>).
1
6> word_sensitive_nif:query_cate_weight(Ref, <<"abc">>).
#{1 => 1}
7> 
```

# Common Test

```erlang
   {ok, Ref} = word_sensitive_nif:new(),
    ok = word_sensitive_nif:add_key_word(Ref, <<"abc">>),
    word_sensitive_nif:build(Ref),
    Result = word_sensitive_nif:query(Ref, <<"abc">>),
    ?assertEqual([<<"abc">>], Result),
    ?assertEqual(1, word_sensitive_nif:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1}, word_sensitive_nif:query_cate_weight(Ref, <<"abc">>)),

    word_sensitive_nif:add_key_word_ext(Ref,
                                        <<"bc">>,
                                        #ext{cate = 2,
                                             len = 2,
                                             weigh = 3}),
    word_sensitive_nif:add_key_word_ext(Ref,
                                        <<"cd">>,
                                        #ext{cate = 1,
                                             len = 2,
                                             weigh = 3}),

    word_sensitive_nif:build(Ref),
    ?assertEqual(4, word_sensitive_nif:query_total_weight(Ref, <<"abc">>)),
    ?assertEqual(#{1 => 1, 2 => 3}, word_sensitive_nif:query_cate_weight(Ref, <<"abc">>)),
    ?assertEqual(7, word_sensitive_nif:query_total_weight(Ref, <<"abcd">>)),
    ?assertEqual(#{1 => 4, 2 => 3}, word_sensitive_nif:query_cate_weight(Ref, <<"abcd">>)),

    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive_nif:query(Ref, <<"abcd">>)),

    ok = word_sensitive_nif:add_key_word(Ref, <<"历史"/utf8>>),
    ok = word_sensitive_nif:add_key_word(Ref, <<"物理"/utf8>>),
    word_sensitive_nif:build(Ref),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"cd">>], word_sensitive_nif:query(Ref, <<"abcd">>)),
    ?assertEqual([<<"历史"/utf8>>], word_sensitive_nif:query(Ref, <<"我要上历史课"/utf8>>)),
    ?assertEqual([<<"abc">>, <<"bc">>, <<"历史"/utf8>>],
                 word_sensitive_nif:query(Ref, <<"abc我要上历史课"/utf8>>)),
```

# Reference

[word_sensitive](https://github.com/yangcancai/word_sensitive)