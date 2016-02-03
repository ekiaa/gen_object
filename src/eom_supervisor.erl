-module(eom_supervisor).

-behaviour(gen_object).

% gen_object
-export([inherit/1, init/2, handle_call/2, handle_info/2, terminate/2]).

% Public
-export([]).

% Private
-export([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%===============================================================================

inherit(Params) ->
	{gen_object, Params}.

%-------------------------------------------------------------------------------

init(#{childs := Childs}, Object) ->
	do_init({start_child, Childs}, Object#{childs => #{}, pids => #{}}).

%-------------------------------------------------------------------------------

handle_call({start_child, Spec}, Object) ->
	do_start_child(Spec, Object);

handle_call(_Msg, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

handle_info(_Info, _Object) ->
	appeal.

%-------------------------------------------------------------------------------

terminate(_Reason, #{registry := Registry}) ->
	ok.

%===============================================================================

do_init({start_child, []}, Object) ->
	{ok, Object};

do_init({start_child, [{ID, Class, Params, Restart, Shutdown} | Rest]}, #{childs := Childs, pids := Pids} = Object) ->
	case gen_object:start_link(Class, Params) of
		{ok, Pid} ->
			do_init({start_child, Rest}, Object#{
				childs => maps:put(ID, {Class, Params, Restart, Shutdown}, Childs),
				pids => maps:put(Pid, ID, Pids)
			});
		{error, Reason} ->
			{error, Reason};
		Result ->
			{error, Result}
	end.

%-------------------------------------------------------------------------------

do_start_child({ID, Class, Params, Restart, Shutdown}, Object) ->
	do_start_child({check_id, {ID, Class, Params, Restart, Shutdown}}, Object);

do_start_child({check_id, {ID, Class, Params, Restart, Shutdown}}, #{childs := Childs} = Object) ->
	case maps:is_key(ID, Childs) of
		false ->
			do_start_child({start_child, {ID, Class, Params, Restart, Shutdown}}, Object);
		true ->
			{reply, {error, id_already_started}}
	end;

do_start_child({start_child, {ID, Class, Params, Restart, Shutdown}}, #{childs := Childs, pids := Pids} = Object) ->
	case catch gen_object:start_link(Class, Params) of
		{ok, Pid} ->
			{reply, {ok, Pid}, Object#{
				childs => maps:put(ID, {Class, Params, Restart, Shutdown}, Childs),
				pids => maps:put(Pid, ID, Pids)
			}};
		{error, Reason} ->
			{reply, {error, Reason}};
		Result ->
			{reply, {error, Result}}
	end.
	