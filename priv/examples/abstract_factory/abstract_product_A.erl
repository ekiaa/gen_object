-module(abstract_product_A).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_msg/2, terminate/2]).

-export([increment/1, decrement/1]).

increment(Obj) ->
	#{increment := Result} = gen_object:call(Obj, increment),
	Result.

decrement(Obj) ->
	#{decrement := Result} = gen_object:call(Obj, decrement),
	Result.

inherit() ->
	gen_object.

init(_Params, _Object) ->
	#{
		counter => 0
	}.

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