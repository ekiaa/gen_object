-module(gen_object).

-export([new/2, start_link/2, call/2, call/3, cast/2, delete/1, inherit/1, inherit/3, init/2]).

-export([system_continue/3, system_terminate/4, write_debug/3, system_get_state/1, system_replace_state/2, behaviour_info/1]).

-export([write_debug/4, loop/3]).

-ifdef(debug).
-define(DEBUG(Format, Params), io:format("DEBUG [~p:~p] " ++ Format ++ "~n", [?MODULE, ?LINE] ++ Params)).
-else.
-define(DEBUG(Format, Params), ok).
-endif.

% -callback constructor(Args :: term()) -> {'ok', Object :: map()}.
% -callback method(Method :: atom() | {atom(), Args :: term()}, Object :: map()) -> {'return', NewObject :: map()}

new(Class, Params) ->
	case start_link(Class, Params) of
		{ok, Pid} -> Pid;
		Result -> Result
	end.

start_link(Class, Params) ->
	proc_lib:start_link(?MODULE, init, [self(), {Class, Params}]).

call(Pid, Method) when is_pid(Pid) ->
	call(Pid, Method, 5000).

call(Pid, Method, Timeout) when is_pid(Pid), is_integer(Timeout), Timeout > 0; Timeout == infinity ->
	Id = erlang:make_ref(),
	Pid ! {call, Method, self(), Id},
	receive
		{Id, Result} -> Result
	after
		Timeout -> {error, timeout}
	end.

cast(Pid, Method) when is_pid(Pid) ->
	Pid ! {cast, Method},
	ok.

delete(Pid) when is_pid(Pid) ->
	Pid ! {delete, self()},
	ok;
delete(_) ->
	{error, not_matched}.

inherit(Class) ->
	#{ref => erlang:make_ref(), inheritance => maps:put(Class, ?MODULE, #{})}.

inherit(Class, BaseClass, Params) ->
	case BaseClass:init(Params) of
		{return, #{inheritance := Inheritance} = Object} ->
			Object#{inheritance => maps:put(Class, BaseClass, Inheritance)};
		Result ->
			erlang:error({error, {bad_return, {?MODULE, ?LINE, inherit, {{BaseClass, init, [Params]}, Result}}}}, [Class, BaseClass, Params])
	end.

behaviour_info(callbacks) ->
	[
		{init, 1},
		{handle_msg, 2},
		{terminate, 2}
	].

init(Parent, {Class, Params}) ->
	Deb = sys:debug_options([]),
	case catch Class:init(Params) of
		{return, Object} ->
			proc_lib:init_ack(Parent, {ok, self()}),
			loop(Object#{class => Class}, Parent, Deb);
		Result ->
			erlang:error({error, {bad_return, {?MODULE, ?LINE, init, {{Class, init, [Params]}, Result}}}}, [Parent, {Class, Params}])
	end.			

loop(#{class := Class} = Object, Parent, Deb) ->
	receive
		{call, Message, From, Id} ->
			do_call(Message, From, Id, Object, Parent, Deb);
		{cast, Message} ->
			case handle_msg(Class, Message, Object) of
				{return, _Result} ->
					loop(Object, Parent, Deb);
				{return, _Result, NewObject} ->
					loop(NewObject, Parent, Deb);
				Result ->
					erlang:error({error, {bad_return, {?MODULE, ?LINE, loop, {{Class, handle_msg, [Message, Object]}, Result}}}}, [Object, Parent, Deb])
			end;
		{system, From, Request} = _Msg ->
			?DEBUG("loop -> receive ~p", [_Msg]),
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, Object);
		{delete, From} ->
			?DEBUG("loop -> receive delete from ~p for Class: ~p", [From, Class]),
			case catch Class:terminate(Object, From) of
				{'EXIT', ErrorMsg} ->
					erlang:exit(ErrorMsg);
				_ ->
					erlang:exit(normal)
			end;
		_Message ->
			?DEBUG("loop -> receive Message: ~p", [_Message]),
			loop(Object, Parent, Deb)
	after
		30000 ->
		%	?DEBUG("loop -> after 30000 ms process hibernated", []),
			proc_lib:hibernate(?MODULE, loop, [Object, Parent, Deb])
	end;

do_call(Message, From, Id, Object, Parent, Deb);
	case handle_msg(Class, Message, Object) of
		{return, Result} ->
			From ! {Id, Result},
			loop(Object, Parent, Deb);
		{return, Result, NewObject} ->
			From ! {Id, Result},
			loop(NewObject, Parent, Deb);
		Result ->
			From ! {Id, {error, bad_return}},
			erlang:error({error, {bad_return, {?MODULE, ?LINE, loop, {{Class, handle_msg, [Message, Object]}, Result}}}}, [Object, Parent, Deb])
	end.

handle_msg(?MODULE, {Key, Value}, Object) when is_atom(Key); is_binary(Key) ->
	{return, ok, maps:put(Key, Value, Object)};

handle_msg(?MODULE, Key, Object) when is_atom(Key); is_binary(Key) ->
	{return, maps:get(Key, Object, undefined)};

handle_msg(?MODULE, Message, _Object) ->
	{return, {error, {not_matched, Message}}};

handle_msg(Class, Message, #{inheritance := Inheritance} = Object) ->
	case catch Class:handle_msg(Message, Object) of
		appeal -> 
			case maps:get(Class, Inheritance) of
				?MODULE ->
					handle_msg(?MODULE, Message, Object);
				BaseClass when is_atom(BaseClass) ->
					handle_msg(BaseClass, Message, Object);
				Result ->
					erlang:error({error, {not_matched, {?MODULE, ?LINE, handle_msg, {{maps, get, [Class, Inheritance]}, Result}}}})
			end;
		Result ->
			Result
	end.

loop(Object, Parent, Deb) ->
	?DEBUG("loop -> not matchecd Object: ~p; Parent: ~p; Deb: ~p", [Object, Parent, Deb]),
	erlang:error({error, {not_matched, {?MODULE, ?LINE, loop, [Object, Parent, Deb]}}}, [Object, Parent, Deb]).

system_continue(Parent, Deb, Object) ->
	?DEBUG("system_continue -> Parent: ~p; Deb: ~p; Object: ~p", [Parent, Deb, Object]),
	loop(Object, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _Object) ->
	?DEBUG("system_terminate -> Reason: ~p; Parent: ~p; Deb: ~p; Object: ~p", [Reason, _Parent, _Deb, _Object]),
	exit(Reason).

system_get_state(Object) ->
	?DEBUG("system_get_state -> Object: ~p", [Object]),
	{ok, Object, Object}.

system_replace_state(StateFun, Object) ->
	?DEBUG("system_replace_state -> StateFun: ~p; Object: ~p", [StateFun, Object]),
	NewState = StateFun(Object),
	{ok, NewState, NewState}.

write_debug(Module, Line, Format, Params) ->
	io:format("DEBUG [~p:~p] " ++ Format ++ "~n", [Module, Line] ++ Params).

write_debug(Dev, Event, Name) ->
	io:format(Dev, "~p event = ~p~n", [Name, Event]).
