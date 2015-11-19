-module(testobj).

-behaviour(gen_object).

-export([inherit/0, init/2, handle_msg/2, terminate/2]).

-export([start/2, do_step4/3]).

start(Pid, Obj2) ->
	gen_object:call(Pid, {start, Obj2}).

inherit() ->
	gen_object.

init(#{key1 := _Key1} = Params, Object) ->
	maps:merge(Object#{res => 0}, Params);

init(Params, Object) ->
	maps:merge(Object, Params).

handle_msg({sum, A, B}, _Object) when is_integer(A), is_integer(B) ->
	{return, A+B};

handle_msg({start, Obj2}, Object) ->
	io:format("[testobj:handle_msg] start; Obj2: ~p~n", [Obj2]),
	do_start(Obj2, Object);

handle_msg({step2, Key2}, #{key1 := Key1}) ->
	Res = Key1 * Key2,
	io:format("[testobj:handle_msg] step2; Key1: ~p; Key2: ~p; Res: ~p~n", [Key1, Key2, Res]),
	{return, Res}.

% handle_msg(_Msg, _Object) ->
% 	appeal.

terminate(_Reason, _Object) ->
	ok.

do_start(Obj2, #{key1 := Key1} = Object) ->
	Ref = testobj2:step1({async, Obj2}, Key1, self()),
	Key4 = 2 * Key1,
	io:format("[testobj:do_start] Obj2: ~p; Key1: ~p; Key4: ~p; Ref: ~p~n", [Obj2, Key1, Key4, Ref]),
	{await, Ref, do_step4, #{key4 => Key4}, Object}.

do_step4(Sum, #{key4 := Key4}, Object) ->
	Res = Sum * Key4,
	io:format("[testobj:do_step4] Sum: ~p; Key4: ~p~n", [Sum, Key4]),
	{return, ok, Object#{res => Res}}.