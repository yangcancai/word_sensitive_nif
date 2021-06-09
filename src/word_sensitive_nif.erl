-module(word_sensitive_nif).

%% API
-export([new/0, new/1, add_key_word/2, build/1, query/2, clear/1]).  %% new resource

            %% clear resource

%% Native library support
-export([load/0]).

-on_load load/0.

-type key_words() :: [binary()].

-opaque word_sensitive() :: reference().

-export_type([word_sensitive/0]).

new() ->
    new(#{}).

-spec new(_Opts :: map()) -> {ok, Ref :: word_sensitive()} | {error, Reason :: binary()}.
new(_Opts) ->
    not_loaded(?LINE).

-spec add_key_word(Ref :: word_sensitive(), Keywords :: key_words()) -> ok.
add_key_word(_Ref, _Keywords) ->
    not_loaded(?LINE).

-spec build(Ref :: word_sensitive()) -> ok.
build(_Ref) ->
    not_loaded(?LINE).

-spec query(Ref :: word_sensitive(), Text :: binary()) -> ok.
query(_Ref, _Text) ->
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
