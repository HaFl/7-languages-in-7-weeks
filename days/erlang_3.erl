-module(erlang_3).
-export([translate/0, translate/2, roulette/0, coroner/0, doctor/0]).
-export([translate_doctor/0, self_healing_doc/0, monitored_doctor/0,
		 start_monitored_doctor/0, doctor_monitor/0, start_doctor_monitor/0]).

translate() ->
	receive
		{Pid, "casa"} ->
			Pid ! "house",
			translate();

		{Pid, "blanca"} ->
			Pid ! "white",
			translate();

		{Pid, "idiota"} ->
			exit({Pid, "You have no manners!"});

		{Pid, _} ->
			Pid ! "I don't understand.",
			translate()
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
			io:format("The shooter ~p died with reason: ~p.", [From, Reason]),
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


% FIND

% restarting OTP service: supervisors --> http://learnyousomeerlang.com/supervisors
% documentation for simple OTP server: http://learnyousomeerlang.com/what-is-otp#the-basic-server
%	definitely worth a look if one is seriously working with Erlang!


% DO

% 1.
translate_doctor() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process.~n"),
			register(translator, spawn_link(fun erlang_3:translate/0)),
			translate_doctor();

		{'EXIT', From, {Pid, Reason}} ->
			io:format("The translation service ~p died with reason ~p.~n", [From, Reason]),
			io:format("Starting another one...~n"),
			self() ! new,
			Pid ! undefined,
			translate_doctor()
	end.

% 2.
roulette_new() ->
	receive
		3 -> io:format("bang.~n"), exit(revolver);
		_ -> io:format("click~n"), roulette_new()
	end.

% kill this Doctor with: exit(Doctor, doctor).
self_healing_doc() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Creating and monitoring process.~n"),
			register(revolver, spawn_link(fun roulette_new/0)),
			self_healing_doc();

		{'EXIT', From, Reason} ->
			case Reason of
				revolver ->
					io:format("The shooter ~p died.~n", [From]),
					io:format("Start another one.~n"),
					self() ! new,
					self_healing_doc();
				doctor ->
					io:format("The doctor ~p died.~n", [From]),
					exit(whereis(revolver), revolver),
					NewDoctor = spawn(fun self_healing_doc/0),
					NewDoctor ! new
			end
	end.

% 3.
start_revolver() ->
	register(revolver, spawn(fun roulette/0)).

start_monitored_doctor() ->
	register(doctor, spawn(fun monitored_doctor/0)),
	doctor ! new.

monitored_doctor() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Taking care of revolver.~n"),
			case whereis(revolver) of
				undefined -> start_revolver();
				_ -> io:format("revolver already exists~n")
			end,
			link(whereis(revolver)),
			io:format("Taking care of monitor.~n"),
			case whereis(monitor) of
				undefined -> start_doctor_monitor();
				_ -> io:format("monitor already exists~n")
			end,
			link(whereis(monitor)),
			monitored_doctor();

		die ->
			io:format("Oh no, not now!"),
			exit("Just five more minutes of being a doctor?!");

		{'EXIT', From, Reason} ->
			io:format("The process ~p died with reason: ~p.~n", [From, Reason]),
			io:format("Start another one.~n"),
			self() ! new,
			monitored_doctor()
	end.

start_doctor_monitor() ->
	register(monitor, spawn(fun doctor_monitor/0)),
	monitor ! new.

doctor_monitor() ->
	process_flag(trap_exit, true),
	receive
		new ->
			io:format("Taking care of doctor.~n"),
			case whereis(doctor) of
				undefined -> start_monitored_doctor();
				_ -> io:format("doctor already exists~n")
			end,
			link(whereis(doctor)),
			doctor_monitor();

		die ->
			io:format("Oh no!"),
			exit("Who will monitor the doctor now?");

		{'EXIT', From, Reason} ->
			io:format("The monitored doctor ~p died with reason ~p.", [From, Reason]),
			io:format("Start another one.~n"),
			self() ! new,
			doctor_monitor()
	end.
