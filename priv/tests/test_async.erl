-module(test_async).

-behaviour(gen_object).

-export([inherit/1, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([test/1, do_test/2]).

test(Term) ->
	{ok, Term}.

inherit(Params) ->
	{gen_object, Params}.

init(_Params, Object) ->
	{ok, Object}.

handle_call(Term, Object) ->
	do_test({request, Term}, Object).

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.

do_test({request, Term}, _Object) ->
	{async, {?MODULE, test, [Term]}, {do_test, result}};

do_test({result, Result}, _Object) ->
	{reply, Result}.