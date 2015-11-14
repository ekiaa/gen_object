-module(testobj).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_msg/2, terminate/2]).

inherit() ->
	gen_object.

init(Params, Object) ->
	maps:merge(Object, Params).

handle_msg({sum, A, B}, _Object) when is_integer(A), is_integer(B) ->
	{return, A+B}.

% handle_msg(_Msg, _Object) ->
% 	appeal.

terminate(_Reason, _Object) ->
	ok.
