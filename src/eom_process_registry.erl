-module(eom_process_registry).

-behaviour(gen_object).

% gen_object
-export([inherit/1, init/2, handle_call/2, handle_info/2, terminate/2]).

% Public
-export([start/1, stop/1, register/3, reserve/2, unregister/2, lookup/2]).

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
	gen_object:call(Registry, {register, Name, Process, self()}).

reserve(Registry, Name) ->
	gen_object:call(Registry, {reserve, Name, self()}).

unregister(Registry, NameOrProcess) ->
	gen_object:call(Registry, {unregister, NameOrProcess}).

lookup(Registry, Process) when is_atom(Registry), is_pid(Process) ->
	case lookup_process(Registry, Process) of
		{ok, NameList, _Ref} ->
			{ok, NameList};
		Result ->
			Result
	end;

lookup(Registry, Name) when is_atom(Registry) ->
	lookup_name(Registry, Name).

%===============================================================================

inherit(Params) ->
	{gen_object, Params}.

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

handle_call({register, Name, Process, Client}, #{registry := Registry}) when is_pid(Process)  ->
	case lookup_name(Registry, {'$reserve', Name}) of
		{ok, Client} ->
			delete_name(Registry, {'$reserve', Name}),
			Result = insert_name(Registry, Name, Process),
			{reply, Result};
		{ok, _} ->
			{reply, {error, already_exists}};
		{error, not_found_name} ->
			Result = insert_name(Registry, Name, Process),
			{reply, Result}
	end;

handle_call({reserve, Name, Client}, #{registry := Registry}) ->
	case lookup_name(Registry, Name) of
		{ok, _} ->
			{reply, {error, already_exists}};
		{error, not_found_name} ->
			Result = insert_name(Registry, {'$reserve', Name}, Client),
			{reply, Result}
	end;

handle_call({unregister, Process}, #{registry := Registry}) when is_pid(Process) ->
	Result = delete(Registry, Process),
	{reply, Result};

handle_call({unregister, Name}, #{registry := Registry}) ->
	Result = delete_name(Registry, Name),
	{reply, Result};

handle_call(_Msg, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

handle_info({'DOWN', _Ref, _Type, Process, _Info}, #{registry := Registry}) ->
	delete(Registry, Process),
	noreply;

handle_info(_Info, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

terminate(_Reason, #{registry := Registry}) ->
	ets:delete(Registry),
	ok.

%===============================================================================

insert_name(Registry, Name, Process) ->
	case ets:insert_new(Registry, {Name, Process}) of
		true ->
			add_name_for_process(Registry, Name, Process);
		false ->
			{error, already_exists}
	end.

add_name_for_process(Registry, Name, Process) ->
	case lookup_process(Registry, Process) of
		{ok, NameList, Ref} ->
			ets:insert(Registry, {Process, [Name | NameList], Ref}),
			ok;
		{error, _Reason} ->
			Ref = erlang:monitor(process, Process),
			ets:insert(Registry, {Process, [Name], Ref}),
			ok
	end.

lookup_name(Registry, Name) ->
	case ets:lookup(Registry, Name) of
		[] ->
			{error, not_found_name};
		[{Name, Process}] ->
			{ok, Process}
	end.

lookup_process(Registry, Process) ->
	case ets:lookup(Registry, Process) of
		[] ->
			{error, not_found_process};
		[{Process, NameList, Ref}] ->
			{ok, NameList, Ref}
	end.

delete(Registry, Process) ->
	case ets:lookup(Registry, Process) of
		[] ->
			{error, not_found_process};
		[{Process, NameList, Ref}] ->
			erlang:demonitor(Ref),
			ets:delete(Registry, Process),
			[ets:delete(Registry, N) || N <- NameList],
			ok
	end.

delete_name(Registry, Name) ->
	case lookup_name(Registry, Name) of
		{ok, Process} ->
			ets:delete(Registry, Name),
			delete_name_for_process(Registry, Name, Process);
		Result ->
			Result
	end.

delete_name_for_process(Registry, Name, Process) ->
	case lookup_process(Registry, Process) of
		{ok, NameList, Ref} ->
			case [N || N <- NameList, N /= Name] of
				[] ->
					erlang:demonitor(Ref),
					ets:delete(Registry, Process),
					ok;
				List ->
					ets:insert(Registry, {Process, List, Ref}),
					ok
			end;
		Result ->
			Result
	end.

%===============================================================================

-ifdef(TEST).

gen_object_test_() ->
	Registry = test_registry,
	{foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{"eom_process_registry: start, stop",
				fun() ->
					?assertMatch({ok, Pid} when is_pid(Pid), eom_process_registry:start(Registry)),
					?assertMatch(Registry, ets:info(Registry, name)),
					eom_process_registry:stop(Registry),
					timer:sleep(10),
					?assertMatch(undefined, ets:info(Registry, name))
				end
			},
			{"eom_process_registry: register",
				fun() ->
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					?assertMatch(ok, eom_process_registry:register(Registry, test_1, Process)),
					?assertMatch(ok, eom_process_registry:register(Registry, test_2, Process)),
					?assertMatch({error, already_exists}, eom_process_registry:register(Registry, test_1, Process)),
					eom_process_registry:stop(Registry)
				end
			},
			{"eom_process_registry: reserve",
				fun() ->
					Process = self(),
					{ok, _} = eom_process_registry:start(Registry),
					?assertMatch(ok, eom_process_registry:reserve(Registry, test)),
					?assertMatch({error, not_found_name}, eom_process_registry:lookup(Registry, test)),
					?assertMatch({ok, Process}, eom_process_registry:lookup(Registry, {'$reserve', test})),
					?assertMatch(ok, eom_process_registry:register(Registry, test, Process)),
					?assertMatch({ok, Process}, eom_process_registry:lookup(Registry, test)),
					eom_process_registry:stop(Registry)
				end
			},
			{"eom_process_registry: unregister",
				fun() ->
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
			},
			{"eom_process_registry: monitor",
				fun() ->
					{ok, _} = eom_process_registry:start(Registry),
					Process = spawn(fun() -> receive stop -> ok end end),
					eom_process_registry:register(Registry, test, Process),
					Process ! stop,
					timer:sleep(10),
					?assertMatch({error, not_found_name}, eom_process_registry:lookup(Registry, test)),
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