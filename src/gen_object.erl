-module(gen_object).

-export([new/2, start_link/2, call/2, call/3, cast/2, delete/1, inherit/1, inherit/3, init/2, loop/1, handle_msg/2]).

-export([system_continue/3, system_terminate/4, system_get_state/1, system_replace_state/2, behaviour_info/1]).

% -export([write_debug/4, loop/3]).

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

cast(Pid, Message) when is_pid(Pid) ->
	Pid ! Message,
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
			loop(#{
				object => Object#{class => Class},
				parent => Parent,
				deb => Deb,
				message => undefined,
				stack => [],
				result => undefined,
				call_result => #{},
				type => undefined,
				from => undefined,
				id => undefined
			});
			% loop(Object#{class => Class}, Parent, Deb);
		Result ->
			erlang:error({error, {bad_return, {?MODULE, ?LINE, init, {{Class, init, [Params]}, Result}}}}, [Parent, {Class, Params}])
	end.			

% loop(#{class := Class} = Object, Parent, Deb) ->
loop(Params) ->
	receive
		{call, Message, From, Id} ->
			preprocessing(Params#{type => call, message => Message, from => From, id => Id});
		{system, From, Request} = _Msg ->
			% ?DEBUG("loop -> receive ~p", [_Msg]),
			#{parent := Parent, deb := Deb} = Params,
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, Params);
		{delete, From} ->
			% ?DEBUG("loop -> receive delete from ~p for Class: ~p", [From, Class]),
			#{object := #{class := Class} = Object} = Params,
			case catch Class:terminate(Object, From) of
				{'EXIT', ErrorMsg} ->
					erlang:exit(ErrorMsg);
				_ ->
					erlang:exit(normal)
			end;
		Message ->
			preprocessing(Params#{type := cast, message => Message})
	after
		30000 ->
		%	?DEBUG("loop -> after 30000 ms process hibernated", []),
			proc_lib:hibernate(?MODULE, loop, [Params])
	end.

preprocessing(#{message := [], stack := Stack} = Params) ->
	reprocess(Params);
preprocessing(#{message := [Message | Messages], stack := Stack} = Params) ->
	preprocessing(Params#{message => Message, stack => [Messages | Stack]});
preprocessing(#{message := #{} = Map} = Params) ->
	preprocessing(Params#{message => maps:to_list(Map)});
preprocessing(#{object := #{class := Class}} = Params) ->
	processing(Params#{class => Class}).

processing(#{message := Message, class := Class, object := Object} = Params) ->
	case catch Class:handle_msg(Message, Object) of
		appeal -> 
			#{inheritance := Inheritance} = Object,
			BaseClass = maps:get(Class, Inheritance),
			processing(Params#{class => BaseClass});
		{return, Result} ->
			postprocessing(Params#{result => Result});
		{return, Result, NewObject} ->
			postprocessing(Params#{result => Result, object => NewObject});
		Result ->
			postprocessing(Params#{result => {error, {bad_return, {Class, Message, Result}}}})
	end.

postprocessing(#{message := {Key, _}, result := Result, call_result := CallResult} = Params) when is_atom(Key) ->
	reprocess(Params#{call_result => maps:put(Key, Result, CallResult)});
postprocessing(#{message := Key, result := Result, call_result := CallResult} = Params) when is_atom(Key) ->
	reprocess(Params#{call_result => maps:put(Key, Result, CallResult)});
postprocessing(#{message := Message, result := Result, call_result := CallResult} = Params) ->
	Key = erlang:phash2(Message),
	reprocess(Params#{call_result => maps:put(Key, Result, CallResult)}).

reprocess(#{stack := []} = Params) ->
	endprocess(Params);
reprocess(#{stack := [Messages | Stack]} = Params) ->
	preprocessing(Params#{stack => Stack, message => Messages}).

endprocess(#{type := call, call_result := CallResult, from := From, id := Id} = Params) ->
	Result = case maps:size(CallResult) of
		1 -> [{_, Res}] = maps:to_list(CallResult), Res;
		_ -> CallResult
	end,
	From ! {Id, Result},
	loop(Params);
endprocess(#{} = Params) ->
	loop(Params).

handle_msg({Key, Value}, Object) when is_atom(Key); is_binary(Key) ->
	{return, ok, maps:put(Key, Value, Object)};

handle_msg(Key, Object) when is_atom(Key); is_binary(Key) ->
	{return, maps:get(Key, Object, undefined)};

handle_msg(_Message, _Object) ->
	{return, {error, not_matched}}.

system_continue(_Parent, _Deb, Params) ->
	% ?DEBUG("system_continue -> Parent: ~p; Deb: ~p; Object: ~p", [Parent, Deb, Object]),
	loop(Params).

system_terminate(Reason, _Parent, _Deb, _Params) ->
	% ?DEBUG("system_terminate -> Reason: ~p; Parent: ~p; Deb: ~p; Object: ~p", [Reason, _Parent, _Deb, _Object]),
	exit(Reason).

system_get_state(Params) ->
	% ?DEBUG("system_get_state -> Object: ~p", [Object]),
	{ok, Params, Params}.

system_replace_state(StateFun, Params) ->
	% ?DEBUG("system_replace_state -> StateFun: ~p; Object: ~p", [StateFun, Object]),
	NewParams = StateFun(Params),
	{ok, NewParams, NewParams}.

% write_debug(Module, Line, Format, Params) ->
% 	io:format("DEBUG [~p:~p] " ++ Format ++ "~n", [Module, Line] ++ Params).

% write_debug(Dev, Event, Name) ->
% 	io:format(Dev, "~p event = ~p~n", [Name, Event]).
