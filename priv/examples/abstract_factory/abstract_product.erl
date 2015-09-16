-module(abstract_product).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

-export([increment/1, decrement/1]).

increment(Obj) ->
	gen_object:call(Obj, increment).

decrement(Obj) ->
	gen_object:call(Obj, decrement).

init(_Params) ->
	Object = gen_object:inherit(?MODULE),
	{return, Object#{
		counter => 0
	}}.

handle_msg(increment, #{counter := Counter} = Object) ->
	Result = Counter + 1,
	{return, Result, Object#{counter => Result}};

handle_msg(decrement, #{counter := Counter} = Object) ->
	Result = Counter - 1,
	{return, Result, Object#{counter => Result}};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.