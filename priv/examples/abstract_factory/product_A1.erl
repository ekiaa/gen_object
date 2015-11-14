-module(product_A1).

-behaviour(gen_object).

-export([relationship/1, init/1, init/2, handle_msg/2, terminate/2]).

-export([create/1]).

create(Params) ->
	gen_object:new(?MODULE, Params).

relationship(_Params) ->
	abstract_product_A.

init(_Params) ->
	#{}.

init(_Params, Object) ->
	Object.

handle_msg(increment, #{counter := Counter} = Object) ->
	Result = Counter + 2,
	{return, Result, Object#{counter => Result}};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.