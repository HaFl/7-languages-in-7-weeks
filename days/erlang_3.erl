-module(erlang_3).
-export([loop/0, translate/2, roulette/0, coroner/0, doctor/0]).

loop() ->
	receive
		{Pid, "casa"} ->
			Pid ! "house",
			loop();

		{Pid, "blanca"} ->
			Pid ! "white",
			loop();

		{Pid, _} ->
			Pid ! "I don't understand.",
			loop()
	end.

translate(To, Word) -> 
	To ! {self(), Word},
	receive
		Translation -> Translation
	end.

roulette() ->
	receive
		3 -> io:format("bang.~n"), exit({roulette, die, at, erlang:time()});
		_ -> io:format("click~n"), roulette()
	end.

coroner() ->
	process_flag(trap_exit, true),
	receive
		{monitor, Process} ->
			link(Process),
			io:format("Monitoring process.~n"),
			coroner();

		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.", [From, Reason]),
			io:format("Start another one.~n"),
			coroner()
	end.

doctor() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process.~n"),
			register(revolver, spawn_link(fun erlang_3:roulette/0)),
			doctor();

		{'EXIT', From, Reason} ->
			io:format("The shooter ~p died with reason ~p.", [From, Reason]),
			io:format("Start another one.~n"),
			self() ! new,
			doctor()
	end.
