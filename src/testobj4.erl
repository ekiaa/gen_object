-module(testobj4).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([do_test/4, do_smth/2]).

inherit() ->
	gen_object.

init(Param, Object) ->
	Object#{param => Param}.

handle_call({func, Func}, Object) when is_function(Func) ->
	do_test(Object#{func => Func});

handle_call(_Msg, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.

do_test(Object) ->
	do_test(step1, [], Object).

do_test(step1, List, #{func := Func, param := Param} = Object) ->
	{func, {do_smth, test1}, {do_test, step2}, List, Object#{param => Func(Param)}}.

do_test(step2, Res1, List, #{func := Func, param := Param} = Object) ->
	{func, {?MODULE, do_smth, test2}, {do_test, step3}, [Res1 | List], Object#{param => Func(Param)}};

do_test(step3, Res2, List, _Object) ->
	{reply, lists:reverse([Res2 | List])}.

do_smth(test1, #{param := Param}) ->
	{reply, {res1, Param}};

do_smth(test2, #{param := Param}) ->
	{reply, {res2, Param}}.