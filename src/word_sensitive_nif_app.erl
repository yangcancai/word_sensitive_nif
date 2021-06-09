%%%-------------------------------------------------------------------
%% @doc word_sensitive_nif public API
%% @end
%%%-------------------------------------------------------------------

-module(word_sensitive_nif_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    word_sensitive_nif_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
