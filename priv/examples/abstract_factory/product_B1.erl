-module(product_B1).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_msg/2, terminate/2]).

-export([create/1]).

create(Params) ->
	gen_object:new(?MODULE, Params).

inherit() ->
	abstract_product_B.

init(_Params, Object) ->
	Object.

handle_msg({multiply, Value}, #{multiplier := Multiplier}) ->
	{return, Multiplier * Value + 1};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.