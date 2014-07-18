-module(pistep).
-export([start/0]).
-export([init/0]).
-export([workerInit/2]).
-record(g00,{x,y}).
-record(g01,{x,y,f}).
-record(g02,{x,y,i,j,f}).
-record(g03,{x,y,i,j,f}).

-record(handles,{x,y}).

-define(RES,40). %steps per mm


gpio_init(What,_Dir) ->
	What.

gpio_write(What,Value) ->
	io:format("Wrote ~p to ~p~n",[Value,What] ).


start() ->


	spawn_link(?MODULE, init, []).

	%loop(0, List).

init() ->
	register(gsrv, self()),
	Worker = spawn_link(?MODULE, workerInit, [[14, 15, 17, 18],[22, 23, 24, 25]]),
	waitExecuted(Worker),
	receive
	after
		1000 ->
		ok
	end,
	gsrv(Worker).

gsrv(Worker) ->
	% unnecessary? Remove?
	receive
		G00=#g00{} ->
			io:format("got g00 ~p~n", [G00]),
			handle_g00(G00,Worker);
		G01=#g01{} ->
			io:format("got g01 ~p~n", [G01]),
			handle_g01(G01,Worker);
		G02=#g02{} ->
			io:format("got g02 ~p~n", [G02]),
			handle_g02(G02,Worker);
		G03=#g03{} ->
			io:format("got g03 ~p~n", [G03]),
			handle_g03(G03,Worker);
		G -> 
			io:format("got something else ~p~n", [G]),
			exit("herp")
	end,
	gsrv(Worker).

handle_g00(Cmd,Worker) ->
	Worker ! {linear,
			  Cmd#g00.x,
			  Cmd#g00.y,
			  maximum,
			  move}.

handle_g01(Cmd,Worker) ->
	Worker ! {linear,
			  Cmd#g01.x,
			  Cmd#g01.y,
			  Cmd#g01.f,
			  tool}.

handle_g02(Cmd,Worker) ->
	Worker ! {circular,
			  Cmd#g02.y,
			  Cmd#g02.i,
			  Cmd#g02.j,
			  Cmd#g02.f,
			  cw}.

handle_g03(Cmd,Worker) ->
	Worker ! {circular,
			  Cmd#g03.x,
			  Cmd#g03.y,
			  Cmd#g03.i,
			  Cmd#g03.j,
			  Cmd#g03.f,
			  ccw}.


workerInit(Xlist,Ylist) ->
	X = [ gpio_init(P,out) || P <- Xlist ],
	Y = [ gpio_init(P,out) || P <- Ylist],

	workerLoop(#handles{x=X,y=Y}, 5).

workerLoop(Handles, Feed) ->
	io:format("worker ready~n"),
	gsrv ! {done,self()},
	receive
		Data = {linear,X,Y,F,Mode} ->
			io:format("linear, ~p~n",[Data] ),
			%XXX use saved feed if undef!
			NewF = whichF(Feed,F,Mode),
			State = handle_linear(Handles,X,Y,NewF,Mode),
			workerLoop(State, NewF);
		Data = {circular,X,Y,I,J,F,Dir} ->
			io:format("circle, ~p, NOP~n",[Data] ),
			workerLoop(Handles, Feed);
		{hold} ->
			io:format("hold, ~n",[] ),
			State = handleHold(Handles),
			workerLoop(State, Feed);
		{release} ->
			io:format("release~n"),
			release(Handles),
			workerLoop(Handles, Feed)
	end.

whichF(F_old,F_new,Mode)->
	case Mode of
		tool ->
			F_new;
		move ->
			F_old
	end.

release([A,B,C,D]) ->
	gpio:write(A,0),
	gpio:write(B,0),
	gpio:write(C,0),
	gpio:write(D,0).

handleHold(L) ->
	X = handleHoldLoop(L#handles.x,8,forward),
	Y = handleHoldLoop(L#handles.y,8,forward),
	#handles{x=X,y=Y}.

handleHoldLoop(L,0,_Dir)->
	L;
handleHoldLoop(L,4,forward) ->
	handleHoldLoop(L,4,backward);
handleHoldLoop(List,N,Dir) ->
	[A,B,C,D] = List,
	gpio_write(D,0),
	gpio_write(C,0),
	gpio_write(A,1),
	gpio_write(B,1),
	io:format("~p~p",[A,B]),
	receive
		after 5 ->
			handleHoldLoop(shiftAccordingly(List, Dir),N-1,Dir)
	end.

shiftAccordingly([A,B,C,D], Dir) ->
	case Dir of
		forward ->
			[B,C,D,A];
		backward ->
			[D,A,B,C]
	end.

waitExecuted(Pid) ->
	waitExecuted(Pid, 10000).

waitExecuted(Pid, Time) ->
	receive
		{done, Pid} ->
			ok;
		F ->
			io:format("fail response: ~p ~p", [F,Pid]),
			error("fail response")
	after 
		Time ->
			io:format("fail timeout ~p", [Time]),
			error("fail timeout")
	end.

handle_linear(Handles,X,Y,F,Mode) ->
	io:format("handle linear",[]),
		
	Xa = myabs(X),
	Ya = myabs(Y),

	% assert tool status
	Wait = feedToWait(F,Mode),

	{Xhandles,Yhandles} = case Xa of
		%Y ->
		%	io:format("X == Y",[]);
		_ when Xa>=Ya ->
			io:format("X > Y ~n",[]),
			{A1,A2} = line_to_steps(X,Handles#handles.x,Y,Handles#handles.y,Wait),
			{A1,A2};
		_ when Ya>Xa ->
			io:format("Y > X ~n",[]),
			{A1,A2} = line_to_steps(Y,Handles#handles.y,X,Handles#handles.x,Wait),
			{A2,A1} % le flip 
	end,
	#handles{x=Xhandles,y=Yhandles}.

line_to_steps(A1,A1_handles,A2,A2_handles,W) ->
	% 1st wait here possibly.
	line_to_steps(myabs(A1),A1_handles,dir(A1),myabs(A2),A2_handles,dir(A2),myabs(A2)/myabs(A1),W,0,0).

line_to_steps(0,A1_handles,_,0,A2_handles,_,_Q,_W,_S1,_S2) ->
	{A1_handles,A2_handles};
line_to_steps(0,_,_,A2,_,_,_Q,_F,_S1,_S2) ->
	io:format("missed ~p A2-steps!~n",[A2]),
	error("missed A2-steps!");

line_to_steps(A1,A1_handles,A1_dir,A2,A2_handles,A2_dir,Q,W,S1,S2) ->
	case ((S1+1)*Q)>(0.5+S2) of
		true ->
			case A2 > 0 of
				true ->
					io:format("Step A2~n",[]);
				false ->
					io:format("WARNING we have run out of A2 steps, but more are expected.. A1=~p A2=~p S=~p Q=~p~n",[A1,A2,S1,Q]),
					exit("wtf")
			end,
			io:format("Step A1 after A2~n",[]),
			% wait sqrt(2)*W
			line_to_steps(A1-1,A1_handles,A1_dir,A2-1,A2_handles,A2_dir,Q,W,S1+1,S2+1);
		false ->
			io:format("Step A1 only~n",[]),
			% wait W
			line_to_steps(A1-1,A1_handles,A1_dir,A2,A2_handles,A2_dir,Q,W,S1+1,S2)
	end.


dir(Var) ->
	case Var < 0 of
		true ->
			backward;
		false ->
			forward
	end.
myabs(Var) ->
	case Var < 0 of
		true ->
			-Var;
		false ->
			Var
	end.


feedToWait(F,Mode) ->
	case Mode of
		move ->
			1;
		tool ->
			round(5/F*5)
	end.

dirToRadSign(Dir) ->
	case Dir of
		cw ->
			-1;
		ccw ->
			1
	end.

