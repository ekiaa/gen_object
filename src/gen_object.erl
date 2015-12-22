-module(gen_object).

-export([new/2, start_link/2, call/2, call/3, mcall/2, mcall/3, delete/1, init/2, loop/1]).

-export([handle_call/2, handle_info/2]).

-export([system_continue/3, system_terminate/4, system_get_state/1, system_replace_state/2, behaviour_info/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

behaviour_info(callbacks) ->
	[
		{inherit, 0},
		{init, 2},
		{handle_call, 2},
		{handle_info, 2},
		{terminate, 2}
	].

new(Class, Params) ->
	case start_link(Class, Params) of
		{ok, Pid} -> Pid;
		Result -> Result
	end.

start_link(Class, Params) ->
	proc_lib:start_link(?MODULE, init, [Params, #{parent => self(), class => Class}]).

call(Pid, Message) ->
	call(Pid, Message, 5000).
call({async, Pid}, Message, _Timeout) ->
	send(call, Pid, Message);
call(Pid, Message, Timeout) when is_integer(Timeout), Timeout > 0; Timeout == infinity ->
	Ref = send(call, Pid, Message),
	receive
		{Ref, Result} -> Result
	after
		Timeout -> {error, timeout}
	end.

mcall(Pid, MessageList) ->
	mcall(Pid, MessageList, 5000).
mcall({async, Pid}, MessageList, _Timeout) when is_list(MessageList) ->
	send(mcall, Pid, MessageList);
mcall(Pid, MessageList, Timeout) when is_list(MessageList), is_integer(Timeout), Timeout > 0; Timeout == infinity ->
	Ref = send(mcall, Pid, MessageList),
	receive
		{Ref, Result} -> Result
	after
		Timeout -> {error, timeout}
	end.

send(Type, Pid, Message) when is_pid(Pid); is_atom(Pid) ->
	Ref = erlang:make_ref(),
	Pid ! {Type, Message, self(), Ref},
	Ref.

delete(Pid) when is_pid(Pid); is_atom(Pid) ->
	Pid ! {delete, self()},
	ok.

init(Params, State) ->
	init_relationship(Params, State#{
		deb => sys:debug_options([]), 
		ancestors => #{}, 
		successors => #{},
		stack => #{}}).

init_relationship(Params, #{class := Class} = State) ->
	init_relationship(Class, Params, State).

init_relationship(Successor, Params, #{ancestors := Ancestors, successors := Successors} = State) ->
	case catch Successor:inherit() of
		{'EXIT', Reason} ->
			exit({error, ?LINE, Reason});
		?MODULE ->
			init_object(Params, State#{
				ancestors => maps:put(Successor, ?MODULE, Ancestors),
				successors => maps:put(?MODULE, Successor, Successors)});
		Ancestor when is_atom(Ancestor) ->
			init_relationship(Ancestor, Params, State#{
				ancestors => maps:put(Successor, Ancestor, Ancestors),
				successors => maps:put(Ancestor, Successor, Successors)})
	end.

init_object(Params, #{successors := Successors} = State) ->
	Successor = maps:get(?MODULE, Successors),
	init_object(Successor, Params, State#{object => #{}}).

init_object(Ancestor, Params, #{object := AncestorObject} = State) ->
	case catch Ancestor:init(Params, AncestorObject) of
		{'EXIT', Reason} ->
			exit({error, ?LINE, Reason});
		Result ->
			init_object_result(Result, Ancestor, Params, State)
	end.

init_object_result(Result, Ancestor, _Params, #{parent := Parent, class := Class, stack := Stack} = State) when Ancestor == Class ->
	case Result of
		{ok, SuccessorObject} ->
			proc_lib:init_ack(Parent, {ok, self()}),
			loop(State#{object => SuccessorObject});
		{next, MFA, SuccessorObject} -> 
			proc_lib:init_ack(Parent, {ok, self()}),
			Ref = erlang:make_ref(),
			preprocessing(
				get_process_state(#{ 
					class => Class, 
					type => next, 
					mfa => MFA,
					from => self, 
					ref => Ref}),
				State#{stack => maps:put(Ref, #{state => #{type => loop}}, Stack)});
		{error, Reason} ->
			proc_lib:init_ack(Parent, {error, Reason}),
			exit({error, Reason});
		_ ->
			Reason = {bad_return, ?LINE, Result},
			proc_lib:init_ack(Parent, {error, Reason}),
			exit(Reason)
	end;

init_object_result(Result, Ancestor, Params, #{successors := Successors} = State) ->
	SuccessorObject = case Result of
		{ok, Object} -> Object;
		{next, _, Object} -> Object;
		_ -> exit({bad_return, ?LINE, Result})
	end,
	Successor = maps:get(Ancestor, Successors),
	init_object(Successor, Params, State#{object => SuccessorObject}).

loop(State) ->
	receive
		{Ref, Result} when is_reference(Ref) ->
			resumeprocess(Ref, Result, State);
		{call, Message, From, Ref} ->
			#{class := Class} = State,
			preprocessing(
				get_process_state(#{
					class => Class, 
					type => call, 
					message => Message, 
					from => From, 
					ref => Ref}),
				State);
		{mcall, MessageList, From, Ref} ->
			#{class := Class} = State,
			preprocessing(
				get_process_state(#{
					class => Class, 
					type => mcall, 
					message_list => MessageList,
					result => #{},
					from => From, 
					ref => Ref}),
				State);
		{system, From, Request} = _Msg ->
			#{parent := Parent, deb := Deb} = State,
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
		{delete, _From} ->
			terminate(normal, State);
		Message ->
			#{class := Class} = State,
			preprocessing(
				get_process_state(#{
					class => Class,
					type => info,
					message => Message}),
				State)
	after
		30000 ->
			proc_lib:hibernate(?MODULE, loop, [State])
	end.

preprocessing(#{type := mcall, message_list := []} = ProcessState, State) ->
	endprocess(ProcessState, State);
preprocessing(#{type := mcall, message_list := [Message | MessageList]} = ProcessState, State) ->
	processing(ProcessState#{message => Message, message_list => MessageList}, State);
preprocessing(ProcessState, State) ->
	processing(ProcessState, State).

processing(#{type := info, message := Message, class := Class} = ProcessState, #{object := Object} = State) ->
	case catch Class:handle_info(Message, Object) of
		Res -> resultprocessing(Res, ProcessState, State)
	end;
processing(#{type := Type, message := Message, class := Class} = ProcessState, #{object := Object} = State) when Type == call; Type == mcall ->
	case catch Class:handle_call(Message, Object) of
		Res -> resultprocessing(Res, ProcessState, State)
	end;
processing(#{type := Type, mfa := {Function, Argument}, class := Class} = ProcessState, #{object := Object} = State) when Type == next; Type == func ->
	case catch Class:Function(Argument, Object) of
		Res -> resultprocessing(Res, ProcessState, State)
	end;
processing(#{type := Type, mfa := {Module, Function, Argument}} = ProcessState, #{object := Object} = State) when Type == next; Type == func ->
	case catch Module:Function(Argument, Object) of
		Res -> resultprocessing(Res, ProcessState, State)
	end.

resultprocessing(Res, ProcessState, State) ->
	case Res of
		{'EXIT', {function_clause, _}} ->
			processing_appeal(ProcessState, State);
		appeal -> 
			processing_appeal(ProcessState, State);
		{appeal, Object} -> 
			processing_appeal(ProcessState, State#{object => Object});
		noreply ->
			postprocessing(undefined, ProcessState, State);
		{noreply, Object} ->
			postprocessing(undefined, ProcessState, State#{object => Object});
		{reply, Result} ->
			postprocessing(Result, ProcessState, State);
		{reply, Result, Object} ->
			postprocessing(Result, ProcessState, State#{object => Object});
		{await, Ref, Callback} ->
			suspendprocess(Ref, Callback, ProcessState, State);
		{await, Ref, Callback, Object} ->
			suspendprocess(Ref, Callback, ProcessState, State#{object => Object});
		{next, MFA} ->
			recurprocess(next, MFA, ProcessState, State);
		{next, MFA, Object} ->
			recurprocess(next, MFA, ProcessState, State#{object => Object});
		{func, MFA, Callback} ->
			recurprocess(func, MFA, Callback, ProcessState, State);
		{func, MFA, Callback, Object} ->
			recurprocess(func, MFA, Callback, ProcessState, State#{object => Object});
		{call, Message, Callback} ->
			recurprocess(call, Message, Callback, ProcessState, State);
		{call, Message, Callback, Object} ->
			recurprocess(call, Message, Callback, ProcessState, State#{object => Object});
		{mcall, MessageList, Callback} ->
			recurprocess(mcall, MessageList, Callback, ProcessState, State);
		{mcall, MessageList, Callback, Object} ->
			recurprocess(mcall, MessageList, Callback, ProcessState, State#{object => Object});
		_ ->
			exit({bad_return, ?LINE, Res})
	end.

processing_appeal(#{class := Class} = ProcessState, #{ancestors := Ancestors} = State) ->
	Ancestor = maps:get(Class, Ancestors),
	processing(ProcessState#{class => Ancestor}, State).

postprocessing(Result, #{type := mcall, message := {Key, _}, result := CallResult} = ProcessState, State) when is_atom(Key) ->
	preprocessing(ProcessState#{result => maps:put(Key, Result, CallResult)}, State);
postprocessing(Result, #{type := mcall, message := Key, result := CallResult} = ProcessState, State) when is_atom(Key) ->
	preprocessing(ProcessState#{result => maps:put(Key, Result, CallResult)}, State);
postprocessing(Result, #{type := mcall, message := Message, result := CallResult} = ProcessState, State) ->
	Key = erlang:phash2(Message),
	preprocessing(ProcessState#{result => maps:put(Key, Result, CallResult)}, State);
postprocessing(_Result, #{type := info} = ProcessState, State) ->
	endprocess(ProcessState, State);
postprocessing(_Result, #{type := loop}, State) ->
	loop(State);
postprocessing(Result, #{type := next, from := self, ref := Ref}, #{stack := Stack} = State) ->
	#{state := ProcessState} = maps:get(Ref, Stack),
	postprocessing(Result, ProcessState, State#{stack => maps:remove(Ref, Stack)});
postprocessing(Result, ProcessState, State) ->
	endprocess(ProcessState#{result => Result}, State).

endprocess(#{type := info}, State) ->
	loop(State);
endprocess(#{from := From, ref := Ref, result := Result}, State) ->
	prepare_reply(From, Ref, Result, State).

prepare_reply(ReplyTo, Ref, Result, State) when is_map(Result) ->
	case maps:size(Result) of
		1 ->
			[Value] = maps:values(Result),
			reply(ReplyTo, Ref, Value, State);
		_ ->
			reply(ReplyTo, Ref, Result, State)
	end;
prepare_reply(ReplyTo, Ref, Result, State) ->
	reply(ReplyTo, Ref, Result, State).

reply(self, Ref, Result, State) ->
	resumeprocess(Ref, Result, State);
reply(ReplyTo, Ref, Result, State) ->
	ReplyTo ! {Ref, Result},
	loop(State).

suspendprocess(Ref, Callback, ProcessState, #{stack := Stack} = State) ->
	loop(State#{stack => maps:put(Ref, #{callback => Callback, state => ProcessState}, Stack)}).

resumeprocess(Ref, Result, #{stack := Stack} = State) ->
	case maps:get(Ref, Stack, undefined) of
		undefined ->
			loop(State);
		#{callback := Callback, state := ProcessState} ->
			#{class := Class} = ProcessState,
			#{object := Object} = State,
			case Callback of
				Function when is_atom(Function) ->
					case catch Class:Function(Result, Object) of
						Res ->
							resultprocessing(Res, ProcessState, State#{stack => maps:remove(Ref, Stack)})
					end;
				{Function, Key} when is_atom(Function), is_atom(Key) ->
					case catch Class:Function({Key, Result}, Object) of
						Res ->
							resultprocessing(Res, ProcessState, State#{stack => maps:remove(Ref, Stack)})
					end;
				{Function, Key, Context} when is_atom(Function), is_atom(Key) ->
					case catch Class:Function({Key, Result, Context}, Object) of
						Res ->
							resultprocessing(Res, ProcessState, State#{stack => maps:remove(Ref, Stack)})
					end;
				_ ->
					exit({not_matched, ?LINE, Callback})
			end
	end.

recurprocess(next, MFA, #{type := next} = ProcessState, State) ->
	preprocessing(ProcessState#{mfa => MFA}, State);

recurprocess(next, MFA, #{class := Class} = ProcessState, #{stack := Stack} = State) ->
	Ref = erlang:make_ref(),
	preprocessing(
		get_process_state(#{ 
			class => Class, 
			type => next, 
			mfa => MFA,
			from => self, 
			ref => Ref}),
		State#{stack => maps:put(Ref, #{state => ProcessState}, Stack)}).

recurprocess(func, MFA, Callback, #{class := Class} = ProcessState, #{stack := Stack} = State) ->
	Ref = erlang:make_ref(),
	preprocessing(
		get_process_state(#{ 
			class => Class, 
			type => func, 
			mfa => MFA,
			from => self, 
			ref => Ref}),
		State#{stack => maps:put(Ref, #{callback => Callback, state => ProcessState}, Stack)});

recurprocess(call, Message, Callback, #{class := Class} = ProcessState, #{stack := Stack} = State) ->
	Ref = erlang:make_ref(),
	preprocessing(
		get_process_state(#{
			class => Class, 
			type => call, 
			message => Message,
			from => self, 
			ref => Ref}),
		State#{stack => maps:put(Ref, #{callback => Callback, state => ProcessState}, Stack)});

recurprocess(mcall, MessageList, Callback, #{class := Class} = ProcessState, #{stack := Stack} = State) ->
	Ref = erlang:make_ref(),
	preprocessing(
		get_process_state(#{
			class => Class, 
			type => mcall, 
			message_list => MessageList,
			result => #{},
			from => self, 
			ref => Ref}),
		State#{stack => maps:put(Ref, #{callback => Callback, state => ProcessState}, Stack)}).

handle_call(_Message, _Object) ->
	{reply, undefined}.

handle_info(_Message, _Object) ->
	noreply.

system_continue(_Parent, _Deb, State) ->
	loop(State).

system_terminate(Reason, _Parent, _Deb, State) ->
	terminate(Reason, State).


system_get_state(State) ->
	{ok, State, State}.

system_replace_state(StateFun, State) ->
	NewState = StateFun(State),
	{ok, NewState, NewState}.

terminate(Reason, #{class := Class, object := Object}) ->
	case catch Class:terminate(Reason, Object) of
		{'EXIT', R} ->
			exit(R);
		_ ->
		    case Reason of
				normal ->
					erlang:exit(normal);
				shutdown ->
					erlang:exit(shutdown);
				{shutdown, _} = Shutdown ->
					erlang:exit(Shutdown);
				Reason ->
					erlang:exit(Reason)
			end
	end.

get_process_state(State) ->
	DefaultState = #{
		message => undefined,
		message_list => undefined,
		result => undefined,
		type => undefined,
		from => undefined,
		ref => undefined},
	maps:merge(DefaultState, State).

%===============================================================================

-ifdef(TEST).

gen_object_test_() ->
	{foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{"gen_object: create",
				fun() ->
					?assertMatch(Obj when is_pid(Obj), gen_object:new(testobj, #{})),
					?assertMatch({ok, Obj} when is_pid(Obj), gen_object:start_link(testobj, #{}))
				end
			},
			{"gen_object: call",
				fun() ->
					Obj = gen_object:new(testobj, #{b => 2}),
					?assertMatch(2,  gen_object:call(Obj, b)),
					?assertMatch(undefined, gen_object:call(Obj, [a, b])),
					?assertMatch(undefined, gen_object:call(Obj, a)),
					?assertMatch(ok,  gen_object:call(Obj, {add, {a, 1}})),
					?assertMatch(1, gen_object:call(Obj, a)),
					?assertMatch(ok, gen_object:call(Obj, {a, 2}))
				end
			},
			{"gen_object: mcall",
				fun() ->
					Obj = gen_object:new(testobj, #{b => 2}),
					gen_object:call(Obj, {add, {a, 1}}),
					?assertMatch(#{a := 1, b := 2}, gen_object:mcall(Obj, [a, b])),
					?assertMatch(#{a := ok, b := ok}, gen_object:mcall(Obj, [{a, 3}, {b, 4}])),
					?assertMatch(#{a := 3, b := 4}, gen_object:mcall(Obj, [a, b])),
					?assertMatch(2, begin Res = gen_object:mcall(Obj, [{a, 9}, b]), maps:size(Res) end)
				end
			},
			{"gen_object: async call",
				fun() ->
					Obj = gen_object:new(testobj, #{b => 2}),
					?assertMatch(Ref when is_reference(Ref), gen_object:call({async, Obj}, {add, {a, 1}})),
					?assertMatch(Ref when is_reference(Ref), gen_object:mcall({async, Obj}, [{b, 3}, {a, 4}], infinity)),
					?assertMatch(4, begin Ref = gen_object:call({async, Obj}, a), receive {Ref, Res} -> Res end end),
					?assertMatch(#{a := 4, b := 3}, gen_object:mcall(Obj, [a, b]))
				end
			},
			{"gen_object: phash2",
				fun() ->
					Obj = gen_object:new(testobj, #{}),
					Method = {sum, 1, 2},
					Hash = erlang:phash2(Method),
					?assertMatch(3, begin Res = gen_object:mcall(Obj, [{sum, 1, 2}, a]), maps:get(Hash, Res) end)
				end
			},
			{"gen_object: await",
				fun() ->
					Obj1 = gen_object:new(testobj, #{key1 => 2}),
					Obj2 = gen_object:new(testobj2, noparams),
					gen_object:call(Obj2, {key2, 3}),
					?assertMatch(ok, testobj:start(Obj1, Obj2)),
					?assertMatch(32, gen_object:call(Obj1, res)),
					?assertMatch(8, gen_object:call(Obj2, res))
				end
			},
			{"gen_object: call",
				fun() ->
					Obj = gen_object:new(testobj3, #{}),
					?assertMatch("Params: par1 = no_value; par2 = no_value.", gen_object:call(Obj, get_params_listing)),
					?assertMatch(ok, gen_object:call(Obj, {par1, val1})),
					?assertMatch("Params: par1 = val1; par2 = no_value.", gen_object:call(Obj, get_params_listing)),
					?assertMatch(ok, gen_object:call(Obj, {par2, val2})),
					?assertMatch("Params: par1 = val1; par2 = val2.", gen_object:call(Obj, get_params_listing))
				end
			},
			{"gen_object: info",
				fun() ->
					Obj = gen_object:new(testobj, #{info => info}),
					Obj ! other_msg,
					Obj ! message,
					?assertMatch(message, gen_object:call(Obj, info))
				end
			},
			{"gen_object: func",
				fun() ->
					Obj = gen_object:new(testobj4, 1),
					?assertMatch([{res1, 2}, {res2, 3}], gen_object:call(Obj, {func, fun(I) -> I + 1 end}))
				end
			},
			{"abstract_factory",
				fun() ->
					Factory_1 = concrete_factory_1:create(),
					?assertMatch(Pid when is_pid(Pid), Factory_1),
					
					Product_A1 = abstract_factory:create_product_A(Factory_1, null),
					?assertMatch(Pid when is_pid(Pid), Product_A1),
					ValueA1_1 = abstract_product_A:increment(Product_A1),
					?assertMatch(2, ValueA1_1),
					ValueA1_2 = abstract_product_A:decrement(Product_A1),
					?assertMatch(1, ValueA1_2),

					Product_B1 = abstract_factory:create_product_B(Factory_1, #{multiplier => 2}),
					?assertMatch(true, is_pid(Product_B1)),
					ValueB1 = abstract_product_B:multiply(Product_B1, 2),
					?assertMatch(5, ValueB1),
					
					Factory_2 = concrete_factory_2:create(),
					?assertMatch(true, is_pid(Factory_2)),
					
					Product_A2 = abstract_factory:create_product_A(Factory_2, null),
					?assertMatch(true, is_pid(Product_A2)),
					ValueA2_1 = abstract_product_A:increment(Product_A2),
					?assertMatch(1, ValueA2_1),
					ValueA2_2 = abstract_product_A:decrement(Product_A2),
					?assertMatch(-1, ValueA2_2),

					Product_B2 = abstract_factory:create_product_B(Factory_2, #{multiplier => 3}),
					?assertMatch(true, is_pid(Product_B2)),
					ValueB2 = abstract_product_B:multiply(Product_B2, 3),
					?assertMatch(8, ValueB2)
				end
			}
		]
	}.

setup() ->
	error_logger:tty(false),
	application:start(gen_object).

cleanup(_) ->
	application:stop(gen_object),
	error_logger:tty(true).

-endif.