-module(eom_process_registry).

-behaviour(gen_object).

% gen_object
-export([inherit/0, init/2, handle_call/2, handle_info/2, terminate/2]).

% Public
-export([start/1, stop/1, register/3, unregister/2, lookup/2]).

% Private
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%===============================================================================

start(Registry) when is_atom(Registry) ->
	gen_object:start_link(?MODULE, #{registry => Registry}).

stop(Registry) ->
	gen_object:delete(Registry).

register(Registry, Name, Process) when is_pid(Process) ->
	gen_object:call(Registry, {register, Name, Process}).

unregister(Registry, NameOrProcess) ->
	gen_object:call(Registry, {unregister, NameOrProcess}).

lookup(Registry, Process) when is_atom(Registry), is_pid(Process) ->
	case ets:lookup(Registry, Process) of
		[] ->
			{error, not_found_process};
		[{Process, NameList, _}] ->
			{ok, NameList}
	end;

lookup(Registry, Name) when is_atom(Registry) ->
	case ets:lookup(Registry, Name) of
		[] ->
			{error, not_found_name};
		[{Name, Process}] ->
			{ok, Process}
	end.

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
					{reply, ok};
				[{Process, NameList, Ref}] ->
					ets:insert(Registry, {Process, [Name | NameList], Ref}),
					{reply, ok}
			end;
		false ->
			{reply, {error, already_exists}}
	end;

handle_call({unregister, Process}, #{registry := Registry}) when is_pid(Process) ->
	case ets:lookup(Registry, Process) of
		[] ->
			{reply, {error, not_found_process}};
		[{Process, NameList, Ref}] ->
			erlang:demonitor(Ref),
			ets:delete(Registry, Process),
			[ets:delete(Registry, N) || N <- NameList],
			{reply, ok}
	end;

handle_call({unregister, Name}, #{registry := Registry}) ->
	case ets:lookup(Registry, Name) of
		[] ->
			{reply, {error, not_found_name}};
		[{Name, Process}] ->
			case ets:lookup(Registry, Process) of
				[] ->
					{reply, {error, not_found_process}};
				[{Process, NameList, Ref}] ->
					erlang:demonitor(Ref),
					ets:delete(Registry, Process),
					[ets:delete(Registry, N) || N <- NameList],
					{reply, ok}
			end
	end;

handle_call(_Msg, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

handle_info(_Info, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

terminate(_Reason, #{registry := Registry}) ->
	ets:delete(Registry),
	ok.

%===============================================================================

-ifdef(TEST).

gen_object_test_() ->
	{foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{"eom_process_registry: start, stop",
				fun() ->
					Registry = test_registry,
					?assertMatch({ok, Pid} when is_pid(Pid), eom_process_registry:start(Registry)),
					?assertMatch(Registry, ets:info(Registry, name)),
					eom_process_registry:stop(Registry),
					timer:sleep(10),
					?assertMatch(undefined, ets:info(Registry, name))
				end
			},
			{"eom_process_registry: register",
				fun() ->
					Registry = test_registry,
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					?assertMatch(ok, eom_process_registry:register(Registry, test_1, Process)),
					?assertMatch(ok, eom_process_registry:register(Registry, test_2, Process)),
					?assertMatch({error, already_exists}, eom_process_registry:register(Registry, test_1, Process)),
					eom_process_registry:stop(Registry)
				end
			},
			{"eom_process_registry: unregister",
				fun() ->
					Registry = test_registry,
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					eom_process_registry:register(Registry, test_1, Process),
					?assertMatch(ok, eom_process_registry:unregister(Registry, test_1)),
					eom_process_registry:register(Registry, test_1, Process),
					?assertMatch(ok, eom_process_registry:unregister(Registry, Process)),
					?assertMatch({error, not_found_name}, eom_process_registry:unregister(Registry, test_2)),
					?assertMatch({error, not_found_process}, eom_process_registry:unregister(Registry, Process)),
					eom_process_registry:stop(Registry)
				end
			},
			{"eom_process_registry: lookup",
				fun() ->
					Registry = test_registry,
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					eom_process_registry:register(Registry, test_1, Process),
					?assertMatch({ok, Process}, eom_process_registry:lookup(Registry, test_1)),
					?assertMatch({error, not_found_name}, eom_process_registry:lookup(Registry, test_2)),
					?assertMatch({ok, [test_1]}, eom_process_registry:lookup(Registry, Process)),
					[OtherProcess | _] = erlang:processes(),
					?assertMatch({error, not_found_process}, eom_process_registry:lookup(Registry, OtherProcess)),
					eom_process_registry:register(Registry, test_2, Process),
					?assertMatch({ok, Process}, eom_process_registry:lookup(Registry, test_2)),
					?assertMatch({ok, [test_2, test_1]}, eom_process_registry:lookup(Registry, Process)),
					eom_process_registry:stop(Registry)
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