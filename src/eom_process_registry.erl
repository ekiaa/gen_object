-module(eom_process_registry).

-behaviour(gen_object).

% gen_object
-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

% Public
-export([start/1, register/3, unregister/2, lookup/2]).

% Private
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%===============================================================================

start(Registry) when is_atom(Registry) ->
	gen_object:start_link(?MODULE, #{registry => Registry}).

register(Registry, Name, Process) when is_pid(Process) ->
	gen_object:call(Registry, {register, Name, Process}).

unregister(Registry, Name) ->
	ok.

lookup(Registry, Name) ->
	ok.

%===============================================================================

inherit() ->
	gen_object.

%-------------------------------------------------------------------------------

init(#{registry := Registry}, Object) when is_atom(Registry) ->
	case catch erlang:register(Registry, self()) of
		true ->
			Options = [named_table, {read_concurrency, true}],
			ets:new(Registry, Options),
			{ok, Object#{registry => Registry}};
		Result ->
			{error, Result}
	end.

%-------------------------------------------------------------------------------

handle_call({register, Name, Process}, #{registry := Registry}) when is_pid(Process)  ->
	case ets:insert_new(Registry, {Name, Process}) of
		true ->
			case ets:lookup(Registry, Process) of
				[] ->
					Ref = erlang:monitor(process, Process),
					ets:insert(Registry, {Process, [Name], Ref}),
					{reply, true};
				[{Process, NameList, Ref}] ->
					ets:insert(Registry, {Process, [Name | NameList], Ref}),
					{reply, true}
			end;
		false ->
			{reply, false}
	end;

handle_call(_Msg, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

handle_info(_Info, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

terminate(_Reason, _Object) ->
	ok.

%===============================================================================

-ifdef(TEST).

gen_object_test_() ->
	{foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{"eom_process_registry: start",
				fun() ->
					Registry = test_registry,
					?assertMatch({ok, Pid} when is_pid(Pid), eom_process_registry:start(Registry)),
					?assertMatch(Registry, ets:info(Registry, name))
				end
			},
			{"eom_process_registry: register",
				fun() ->
					Registry = test_registry_1,
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					?assertMatch(true, eom_process_registry:register(Registry, test_1, Process)),
					?assertMatch(true, eom_process_registry:register(Registry, test_2, Process)),
					?assertMatch(false, eom_process_registry:register(Registry, test_1, Process))
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