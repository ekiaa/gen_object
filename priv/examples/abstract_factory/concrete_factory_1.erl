-module(concrete_factory_1).

-behaviour(gen_object).

-export([init/1, handle_msg/2, terminate/2]).

-export([create/0]).

create() ->
	gen_object:new(?MODULE, null).

init(Params) ->
	Object = gen_object:inherit(?MODULE, abstract_factory, Params),
	{return, Object}.

handle_msg({create_product_A, Params}, #{count_A := Count_A} = Object) ->
	Product_A = product_A1:create(Params),
	{return, Product_A, Object#{count_A => Count_A + 1}};

handle_msg({create_product_B, Params}, #{count_B := Count_B} = Object) ->
	Product_B = product_B1:create(Params),
	{return, Product_B, Object#{count_B => Count_B + 1}};

handle_msg(_Message, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.