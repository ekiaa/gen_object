-module(abstract_product_B).

-behaviour(gen_object).

-export([relationship/1, init/1, init/2, handle_msg/2, terminate/2]).

-export([multiply/2]).

multiply(Obj, Value) ->
	gen_object:call(Obj, {multiply, Value}).

relationship(_Params) ->
	gen_object.

init(#{multiplier := Multiplier}) when is_integer(Multiplier) ->
	#{
		multiplier => Multiplier
	}.

init(_Params, Object) ->
	Object.

handle_msg({multiply, Value}, #{multiplier := Multiplier}) ->
	{return, Multiplier * Value};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.