%%%-------------------------------------------------------------------
%% @doc gen_object public API
%% @end
%%%-------------------------------------------------------------------

-module('gen_object_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    'gen_object_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
