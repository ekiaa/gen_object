-module(testobj3).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([do_get_params_listing/2]).

inherit() ->
	gen_object.

init(Params, Object) ->
	{ok, maps:merge(Object, Params)}.

handle_call(par1, Object) ->
	do_get_param(par1, Object);

handle_call(par2, Object) ->
	do_get_param(par2, Object);

handle_call({par1, Par1}, Object) ->
	{reply, ok, Object#{par1 => Par1}};

handle_call({par2, Par2}, Object) ->
	{reply, ok, Object#{par2 => Par2}};

handle_call(get_params_listing, Object) ->
	do_get_params_listing(Object);

handle_call(_Msg, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.

do_get_param(Key, Object) ->
	case maps:get(Key, Object, undefined) of
		undefined -> {reply, "no_value"};
		Val -> {reply, io_lib:format("~p", [Val])}
	end.

do_get_params_listing(Object) ->
	{call, par1, {do_get_params_listing, par1}, #{res => "Params: "}, Object}.

do_get_params_listing({par1, Par1, #{res := Res}}, Object) ->
	NewRes = Res ++ "par1 = " ++ Par1 ++ "; ",
	{call, par2, {do_get_params_listing, par2}, #{res => NewRes}, Object};

do_get_params_listing({par2, Par2, #{res := Res}}, _Object) ->
	NewRes = Res ++ "par2 = " ++ Par2 ++ ".",
	{reply, lists:flatten(NewRes)}.