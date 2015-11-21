-module(testobj2).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([step1/3, do_step3/4]).

step1(Pid, Key1, From) ->
	gen_object:call(Pid, {step1, {Key1, From}}).

inherit() ->
	gen_object.

init(_Params, Object) ->
	Object#{sum => 0}.

handle_call({key2, Value}, Object) ->
	{reply, ok, Object#{key2 => Value}};

handle_call(res, #{res := Value}) ->
	{reply, Value};

handle_call({step1, {Key1, From}}, #{key2 := Key2} = Object) ->
	Ref = gen_object:call({async, From}, {step2, Key2}),
	io:format("[testobj2:handle_call] step1; Key1: ~p; Key2: ~p; From: ~p; Ref: ~p~n", [Key1, Key2, From, Ref]),
	{await, Ref, {do_step3, step3}, #{key1 => Key1}, Object};

handle_call(_Msg, _Object) ->
	appeal.

handle_info(_Info, _Object) ->
	appeal.

terminate(_Reason, _Object) ->
	ok.

do_step3(step3, Key3, #{key1 := Key1}, Object) ->
	Sum = Key1 + Key3,
	io:format("[testobj2:do_step3] Key1: ~p; Key3: ~p; Sum: ~p~n", [Key1, Key3, Sum]),
	{reply, Sum, Object#{res => Sum}}.