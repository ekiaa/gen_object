-module(testobj).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

init(#{} = Params) ->
	Object = gen_object:inherit(?MODULE),
	{return, maps:merge(Object, Params)}.

handle_msg({sum, A, B}, Object) when is_integer(A), is_integer(B) ->
	{return, A+B};

handle_msg(_Msg, _Object) ->
	appeal.

terminate(_, _) ->
	ok.
