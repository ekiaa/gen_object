-module(product_A1).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

-export([create/1]).

create(Params) ->
	gen_object:new(?MODULE, Params).

init(Params) ->
	Object = gen_object:inherit(?MODULE, abstract_product, Params),
	{return, Object}.

handle_msg(increment, #{counter := Counter} = Object) ->
	Result = Counter + 2,
	{return, Result, Object#{counter => Result}};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.