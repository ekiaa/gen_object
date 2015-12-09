-module(abstract_product_B).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([multiply/2]).

multiply(Obj, Value) ->
	gen_object:call(Obj, {multiply, Value}).

inherit() ->
	gen_object.

init(#{multiplier := Multiplier}, _Object) when is_integer(Multiplier) ->
	{ok, #{
		multiplier => Multiplier
	}}.

handle_call({multiply, Value}, #{multiplier := Multiplier}) ->
	{reply, Multiplier * Value};

handle_call(_Message, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.