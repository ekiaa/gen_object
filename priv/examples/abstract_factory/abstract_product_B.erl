-module(abstract_product_B).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

-export([multiply/2]).

multiply(Obj, Value) ->
	gen_object:call(Obj, {multiply, Value}).

init(#{multiplier := Multiplier}) when is_integer(Multiplier) ->
	Object = gen_object:inherit(?MODULE),
	{return, Object#{
		multiplier => Multiplier
	}}.

handle_msg({multiply, Value}, #{multiplier := Multiplier}) ->
	{return, Multiplier * Value};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.