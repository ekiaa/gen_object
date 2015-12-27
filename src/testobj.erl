-module(testobj).

-behaviour(gen_object).

-export([inherit/1, init/2, handle_call/2, handle_info/2, terminate/2]).

-export([start/2, do_step4/2]).

start(Pid, Obj2) ->
	gen_object:call(Pid, {start, Obj2}).

inherit(Params) ->
	{gen_object, Params}.

init(#{key1 := _Key1} = Params, Object) ->
	{ok, maps:merge(Object#{res => 0}, Params)};

init(Params, Object) ->
	{ok, maps:merge(Object, Params)}.

handle_call({sum, A, B}, _Object) when is_integer(A), is_integer(B) ->
	{reply, A+B};

handle_call({add, {Key, Value}}, Object) ->
	{reply, ok, maps:put(Key, Value, Object)};

handle_call({start, Obj2}, Object) ->
	io:format("[testobj:handle_call] start; Obj2: ~p~n", [Obj2]),
	do_start(Obj2, Object);

handle_call({step2, Key2}, #{key1 := Key1}) ->
	Res = Key1 * Key2,
	io:format("[testobj:handle_call] step2; Key1: ~p; Key2: ~p; Res: ~p~n", [Key1, Key2, Res]),
	{reply, Res};

handle_call(Key, Object) when is_atom(Key) ->
	do_get_key_value(Key, Object);

handle_call({Key, Value}, Object) when is_atom(Key) ->
	do_set_key_value(Key, Value, Object).

% handle_call(_Msg, _Object) ->
% 	appeal.

handle_info(message, Object) ->
	{noreply, Object#{info => message}}.

% handle_info(_Info, _Object) ->
% 	appeal.

terminate(_Reason, _Object) ->
	ok.

do_start(Obj2, #{key1 := Key1} = Object) ->
	Ref = testobj2:step1({async, Obj2}, Key1, self()),
	Key4 = 2 * Key1,
	io:format("[testobj:do_start] Obj2: ~p; Key1: ~p; Key4: ~p; Ref: ~p~n", [Obj2, Key1, Key4, Ref]),
	{await, Ref, {do_step4, res, #{key4 => Key4}}, Object}.

do_step4({res, Sum, #{key4 := Key4}}, Object) ->
	Res = Sum * Key4,
	io:format("[testobj:do_step4] Sum: ~p; Key4: ~p~n", [Sum, Key4]),
	{reply, ok, Object#{res => Res}}.

do_get_key_value(Key, Object) ->
	{reply, maps:get(Key, Object, undefined)}.

do_set_key_value(Key, Value, Object) ->
	case maps:is_key(Key, Object) of
		true ->
			{reply, ok, maps:put(Key, Value, Object)};
		false ->
			{reply, undefined}
	end.