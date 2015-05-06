-module(ex).
-export([init/0, create/1, process/1, queue/1, exch_controller/0, listener/1, get_list/0]).

%% initialize the system and register the important processes
init() ->
	register('queue', spawn(ex, queue, [[]])),
	register('controller', spawn(ex, exch_controller, [])),
	Pid = spawn(ex, listener, [[]]),
	register(home, Pid)
	.

%% maintain a list of processes
listener(L) ->
	receive
		{Sender, list} ->
			Sender ! L,
			listener(L);
		Pid -> 
			listener(L ++ [Pid])
	end.

%% return the list of processes to the caller (i.e. the shell)
get_list() ->
	home ! {self(), list},
	receive
		L -> L
	end.

% create N processes (convenience wrapper)
create(N) ->
	create_process(N).
%create(N, M) ->
%	create_process(lists:seq(N,M)).

% create N processes recursively
create_process(0) -> ok;
create_process(N) ->
	Pid = spawn(ex, process, [0]),
	home ! Pid,
	create_process(N-1).

process(V) ->
	io:format("Process ~p now has value ~p ~n", [self(), V]),
	receive 
		exchange ->
			exchange(V);
		{exchange, N} ->
			exchange(N)
	end.

exchange(Value) ->
	'queue' ! {self(), Value},
	receive
		R ->
			process(R)
	end.

exch_controller() ->
	receive 
		{S1, V1} ->
			receive
				{S2, V2} ->
					S1 ! V2,
					S2 ! V1
			end
	end,
	exch_controller().

%% maintain a queue of who is calling the exchange controller,
%% not strictly necessary but avoids unnecessary blocking
queue([]) ->
	receive
		{S,V} ->
			queue([{S,V}])
	end;
queue([H|T]) ->
	receive 
		{Sender, V} ->
			queue([H] ++ T ++ [{Sender,V}])
	after 0 ->
			'controller' ! H,
			queue(T)
	end.
