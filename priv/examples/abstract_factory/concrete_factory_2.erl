-module(concrete_factory_2).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([create/0]).

create() ->
	gen_object:new(?MODULE, null).

inherit() ->
	abstract_factory.

init(_Params, Object) ->
	{ok, Object}.

handle_call({create_product_A, Params}, #{count_A := Count_A} = Object) ->
	Product_A = product_A2:create(Params),
	{reply, Product_A, Object#{count_A => Count_A + 1}};

handle_call({create_product_B, Params}, #{count_B := Count_B} = Object) ->
	Product_B = product_B2:create(Params),
	{reply, Product_B, Object#{count_B => Count_B + 1}};

handle_call(_Message, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.