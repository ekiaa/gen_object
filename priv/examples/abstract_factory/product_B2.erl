-module(product_B2).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

-export([create/1]).

create(Params) ->
	gen_object:new(?MODULE, Params).

init(Params) ->
	Object = gen_object:inherit(?MODULE, abstract_product_B, Params),
	{return, Object}.

handle_msg({multiply, Value}, #{multiplier := Multiplier}) ->
	{return, Multiplier * Value - 1};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.