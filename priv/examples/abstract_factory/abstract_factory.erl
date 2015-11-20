-module(abstract_factory).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([create_product_A/2, create_product_B/2, created_A/1, created_B/1]).

create_product_A(Obj, Params) ->
	gen_object:call(Obj, {create_product_A, Params}).

create_product_B(Obj, Params) ->
	gen_object:call(Obj, {create_product_B, Params}).

created_A(Obj) ->
	gen_object:call(Obj, created_A).

created_B(Obj) ->
	gen_object:call(Obj, created_B).

inherit() ->
	gen_object.

init(_Params, _Object) ->
	#{
		count_A => 0,
		count_B => 0
	}.

handle_call(created_A, #{count_A := Count_A}) ->
	{reply, Count_A};

handle_call(created_B, #{count_B := Count_B}) ->
	{reply, Count_B};

handle_call(_Message, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.