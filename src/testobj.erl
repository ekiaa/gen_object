-module(testobj).

-behaviour(gen_object).

-export([relationship/1, init/1, init/2, handle_msg/2, terminate/2]).

relationship(#{}) ->
	gen_object.

init(#{} = Params) ->
	Params.

init(_, Object) ->
	Object.

handle_msg({sum, A, B}, _Object) when is_integer(A), is_integer(B) ->
	{return, A+B};

handle_msg(_Msg, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.
