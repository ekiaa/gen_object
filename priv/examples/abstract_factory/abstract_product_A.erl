-module(abstract_product_A).

-behaviour(gen_object).

-export([inherit/1, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([increment/1, decrement/1]).

increment(Obj) ->
	gen_object:call(Obj, increment).

decrement(Obj) ->
	gen_object:call(Obj, decrement).

inherit(Params) ->
	{gen_object, Params}.

init(_Params, _Object) ->
	{ok, #{
		counter => 0
	}}.

handle_call(increment, #{counter := Counter} = Object) ->
	Result = Counter + 1,
	{reply, Result, Object#{counter => Result}};

handle_call(decrement, #{counter := Counter} = Object) ->
	Result = Counter - 1,
	{reply, Result, Object#{counter => Result}};

handle_call(_Message, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.