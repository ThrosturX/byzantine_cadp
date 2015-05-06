%%% Nodes are connected in a circle and send a message to the other nodes 
%%% when they enter the barrier.
-module(barr).
-export([start/1, make_node/1, make_node/2]).
-compile([native]).

%% Helper functions
sleep(T) ->
	receive
	after T -> true
	end.
%% Spawns first node
start(N) -> 
	random:seed(now()),
	io:format("Creating barrier with ~w nodes ~n", [N]),
	spawn(barr, make_node, [N]),
	ok.

%% Spawns rest of nodes
make_node(First, 1)->
	io:format("Node 1~n", []),
	sleep(random:uniform(2000)),
	barrier(First, true);	
make_node(First, N) ->
	io:format("Node ~w~n", [N]),
	Pid = spawn(barr, make_node, [First, N-1]),
	sleep(random:uniform(2000)),
	barrier(Pid, true). 

%% Spawns second node
make_node(N) ->
	io:format("Node ~w~n", [N]),
	Pid = spawn(barr, make_node, [self(), N-1]),
	sleep(random:uniform(2000)),
	barrier(Pid, true).

%% When nodes enter the barrier for the first time the send their pid
%% Then they wait for their pid, other pids or a go
barrier(Neighbour, true) ->
	io:format("Node: ~w enters barrier~n", [self()]), 
	Neighbour ! self(),  
	Self = self(),
	receive 
		Self -> 
			Neighbour ! go,
			post_barrier();
			
		N -> 
			Neighbour ! N,
			barrier(Neighbour, false)
	end;
barrier(Neighbour, false) ->
	Self = self(),
	receive 
		go -> 
			Neighbour ! go,
			post_barrier();
		Self -> 
			Neighbour ! go,
			post_barrier();
		N -> 
			Neighbour ! N,
			barrier(Neighbour, false)
	end.

%% When nodes have left the barrier
post_barrier() -> 
	io:format("Node: ~w left barrier~n", [self()]).
