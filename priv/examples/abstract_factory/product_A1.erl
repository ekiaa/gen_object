-module(product_A1).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([create/1]).

create(Params) ->
	gen_object:new(?MODULE, Params).

inherit() ->
	abstract_product_A.

init(_Params, Object) ->
	Object.

handle_call(increment, #{counter := Counter} = Object) ->
	Result = Counter + 2,
	{reply, Result, Object#{counter => Result}};

handle_call(_Message, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.