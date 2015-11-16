-module(gen_object).

-export([new/2, start_link/2, call/2, call/3, cast/2, delete/1, init/2, loop/1, handle_msg/2]).

-export([system_continue/3, system_terminate/4, system_get_state/1, system_replace_state/2, behaviour_info/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

behaviour_info(callbacks) ->
	[
		{inherit, 0},
		{init, 2},
		{handle_msg, 2},
		{terminate, 2}
	].

new(Class, Params) ->
	case start_link(Class, Params) of
		{ok, Pid} -> Pid;
		Result -> Result
	end.

start_link(Class, Params) ->
	proc_lib:start_link(?MODULE, init, [Params, #{parent => self(), class => Class}]).

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

init(Params, State) ->
	init_relationship(Params, State#{
		deb => sys:debug_options([]), 
		ancestors => #{}, 
		successors => #{}}).

init_relationship(Params, #{class := Class} = State) ->
	init_relationship(Class, Params, State).

init_relationship(Successor, Params, #{ancestors := Ancestors, successors := Successors} = State) ->
	case Successor:inherit() of
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

init_object(Ancestor, Params, #{parent := Parent, class := Class, successors := Successors, object := AncestorObject} = State) ->
	case Ancestor:init(Params, AncestorObject) of
		SuccessorObject when Ancestor == Class ->
			proc_lib:init_ack(Parent, {ok, self()}),
			loop(State#{object => SuccessorObject});
		SuccessorObject ->
			Successor = maps:get(Ancestor, Successors),
			init_object(Successor, Params, State#{object => SuccessorObject})
	end.

loop(#{class := Class} = State) ->
	receive
		{call, Message, From, Id} ->
			preprocessing(
				get_process_state(#{
					class => Class, 
					type => call, 
					message => Message, 
					from => From, 
					id => Id}), 
				State);
		{system, From, Request} = _Msg ->
			#{parent := Parent, deb := Deb} = State,
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
		{delete, _From} ->
			terminate(normal, State);
		Message ->
			preprocessing(
				get_process_state(#{
					class => Class, 
					type => cast, 
					message => Message}), 
				State)
	after
		30000 ->
			proc_lib:hibernate(?MODULE, loop, [State])
	end.

preprocessing(#{message := [], stack := []} = ProcessState, State) ->
	reprocess(ProcessState, State);
preprocessing(#{message := [Message | Messages], stack := Stack} = ProcessState, State) ->
	preprocessing(ProcessState#{message => Message, stack => Messages ++ Stack}, State);
preprocessing(#{message := #{} = Map} = ProcessState, State) ->
	preprocessing(ProcessState#{message => maps:to_list(Map)}, State);
preprocessing(ProcessState, State) ->
	processing(ProcessState, State).

processing(#{message := Message, class := Class} = ProcessState, #{object := Object} = State) ->
	case catch Class:handle_msg(Message, Object) of
		{'EXIT', {function_clause, _}} ->
			processing_appeal(ProcessState, State);
		appeal -> 
			processing_appeal(ProcessState, State);
		{return, Result} ->
			postprocessing(ProcessState#{result => Result}, State);
		{return, Result, NewObject} ->
			postprocessing(ProcessState#{result => Result}, State#{object => NewObject});
		Result ->
			postprocessing(ProcessState#{result => {error, {bad_return, {Class, Message, Result}}}}, State)
	end.

processing_appeal(#{class := Class} = ProcessState, #{ancestors := Ancestors} = State) ->
	Ancestor = maps:get(Class, Ancestors),
	processing(ProcessState#{class => Ancestor}, State).

postprocessing(#{message := {Key, _}, result := Result, call_result := CallResult} = ProcessState, State) when is_atom(Key) ->
	reprocess(ProcessState#{call_result => maps:put(Key, Result, CallResult)}, State);
postprocessing(#{message := Key, result := Result, call_result := CallResult} = ProcessState, State) when is_atom(Key) ->
	reprocess(ProcessState#{call_result => maps:put(Key, Result, CallResult)}, State);
postprocessing(#{message := Message, result := Result, call_result := CallResult} = ProcessState, State) ->
	Key = erlang:phash2(Message),
	reprocess(ProcessState#{call_result => maps:put(Key, Result, CallResult)}, State).

reprocess(#{stack := []} = ProcessState, State) ->
	endprocess(ProcessState, State);
reprocess(#{stack := [Messages | Stack]} = ProcessState, State) ->
	preprocessing(ProcessState#{stack => Stack, message => Messages}, State).

endprocess(#{type := call, call_result := Result, from := From, id := Id}, State) ->
	From ! {Id, Result},
	loop(State);
endprocess(#{type := cast}, State) ->
	loop(State).

handle_msg({Key, Value}, Object) when is_atom(Key); is_binary(Key) ->
	{return, ok, maps:put(Key, Value, Object)};

handle_msg(Key, Object) when is_atom(Key); is_binary(Key) ->
	{return, maps:get(Key, Object, undefined)};

handle_msg(_Message, _Object) ->
	{return, {error, not_matched}}.

system_continue(_Parent, _Deb, State) ->
	loop(State).

system_terminate(Reason, _Parent, _Deb, State) ->
	terminate(Reason, State).


system_get_state(State) ->
	{ok, State, State}.

system_replace_state(StateFun, State) ->
	NewState = StateFun(State),
	{ok, NewState, NewState}.

terminate(Reason, #{object := #{class := Class} = Object}) ->
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
		stack => [],
		result => undefined,
		call_result => #{},
		type => undefined,
		from => undefined,
		id => undefined},
	maps:merge(DefaultState, State).

%===============================================================================

-ifdef(TEST).

gen_object_test_() ->
	{foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{"gen_object #1",
				fun() ->
					?assertMatch(Obj when is_pid(Obj), gen_object:new(testobj, #{})),
					?assertMatch({ok, Obj} when is_pid(Obj), gen_object:start_link(testobj, #{}))
				end
			},
			{"gen_object #2",
				fun() ->
					Obj = gen_object:new(testobj, #{b => 2}),
					?assertMatch(#{a := undefined}, gen_object:call(Obj, a)),
					?assertMatch(#{b := 2},  gen_object:call(Obj, b)),
					?assertMatch(#{a := ok}, gen_object:call(Obj, {a, 1})),
					?assertMatch(#{a := 1},  gen_object:call(Obj, a)),
					?assertMatch(#{b := ok}, gen_object:call(Obj, #{b => 3})),
					?assertMatch(#{b := ok}, gen_object:call(Obj, [#{b => 3.5}])),
					?assertMatch(#{a := ok, b := ok}, gen_object:call(Obj, #{b => 4, a => 5})),
					?assertMatch(#{a := 7, b := 6, c := ok}, gen_object:call(Obj, [#{b => 6, a => 7}, a, b, {c, 8}])),
					?assertMatch(2, begin Res = gen_object:call(Obj, [#{x => 9}, y]), maps:size(Res) end)
				end
			},
			{"gen_object #3",
				fun() ->
					Obj = gen_object:new(testobj, #{b => 2}),
					?assertMatch(ok, gen_object:cast(Obj, a)),
					?assertMatch(ok, gen_object:cast(Obj, [#{b => 3}, {a, 4}])),
					?assertMatch(#{a := 4, b := 3}, gen_object:call(Obj, [a, b]))
				end
			},
			{"gen_object #4",
				fun() ->
					Obj = gen_object:new(testobj, #{}),
					Method = {sum, 1, 2},
					Hash = erlang:phash2(Method),
					?assertMatch(3, begin Res = gen_object:call(Obj, [{sum, 1, 2}, a]), maps:get(Hash, Res) end)
				end
			},
			{"abstract_factory",
				fun() ->
					Factory_1 = concrete_factory_1:create(),
					?assertMatch(true, is_pid(Factory_1)),
					
					Product_A1 = abstract_factory:create_product_A(Factory_1, null),
					?assertMatch(true, is_pid(Product_A1)),
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