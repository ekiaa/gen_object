-module(abstract_product_B).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_msg/2, terminate/2]).

-export([multiply/2]).

multiply(Obj, Value) ->
	gen_object:call(Obj, {multiply, Value}).

inherit() ->
	gen_object.

init(#{multiplier := Multiplier}, _Object) when is_integer(Multiplier) ->
	#{
		multiplier => Multiplier
	}.

handle_msg({multiply, Value}, #{multiplier := Multiplier}) ->
	{return, Multiplier * Value};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.