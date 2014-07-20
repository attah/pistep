-module(pistep).
-export([start/0]).
-export([init/0]).
-export([workerInit/2]).
-export ([svg_init/0]).
-record(g00,{x,y}).
-record(g01,{x,y,f}).
-record(g02,{x,y,i,j,f}).
-record(g03,{x,y,i,j,f}).

-record(handles,{x,y}).

-define(RES,40). %steps per mm

-define(SvgHead,"<?xml version=\"1.0\" standalone=\"yes\" ?> <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" \"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd\"> <svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" width=\"1400\" height=\"900\"> <g transform=\"translate(0,900)\"><g transform=\"scale(1,-1)\">").
-define(SvgEnd,"</g></g></svg>").

gpio_init(What,_Dir) ->
	What.

gpio_write(What,Value) ->
	io:format("Wrote ~p to ~p~n",[Value,What] ).

%handlesToAxis(Handles) ->
%	case lists:member(14,Handles) of
%		true

svg_init() ->
	register(svg, self()),
	case file:open("debug.svg", [write]) of
		{ok, Fd} ->
			file:write(Fd, ?SvgHead),
			svg_loop(Fd);
		{error, Reason} ->
			Reason
	end.

svg_loop(Fd) ->
	receive 
		{segment, M, {Xi,Yi}, Xsign, Ysign, A1} ->
			file:write(Fd, io_lib:format("<path d=\"M~p, ~p ", [Xi,Yi])),
			inner_svg_loop(Fd, Xsign, Ysign, A1),
			file:write(Fd, io_lib:format("\" style=\"fill:#ffffff; fill-opacity:0; stroke:~s; stroke-width:1;\"/>",[colorFromMode(M)])),
			svg_loop(Fd);
		{endprogram} ->
			file:write(Fd, ?SvgEnd)
	end.


inner_svg_loop(Fd, Xsign, Ysign, PrimaryAxis) ->
	receive 
		{step, A1, A2} ->
			{X,Y} = case PrimaryAxis of
				x -> {Xsign*A1,Ysign*A2};
				y -> {Xsign*A2,Ysign*A1}
			end,
			file:write(Fd, io_lib:format(" l ~p ~p ",[X,Y])),
			inner_svg_loop(Fd, Xsign, Ysign, PrimaryAxis);
		{endsegment} ->
			ok
	end.

colorFromMode(Mode) ->
	case Mode of
		move ->
			"#000066";
		tool ->
			"#111111"
	end.

start() ->

	spawn_link(?MODULE, svg_init, []),
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
		{done,_Pid} ->
			io:format("Segment done~n",[]);
		G -> 
			io:format("got something else ~p~n", [G]),
			svg ! {endprogram},
			receive
				after 100 ->
					exit("herp")
			end
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
			  Cmd#g02.x,
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

	workerLoop(#handles{x=X,y=Y}, 5, {0,0}).

workerLoop(Handles, Feed, Position) ->
	io:format("worker ready~n"),
	gsrv ! {done,self()},
	receive
		Data = {linear,X,Y,F,Mode} ->
			io:format("linear, ~p~n",[Data] ),
			%XXX use saved feed if undef!
			NewF = whichF(Feed,F,Mode),
			{{Xs,Ys},NewP} = moveFromMode(Position,{X,Y},rel),
			State = handle_linear(Handles,Xs,Ys,NewF,Mode,Position),
			workerLoop(State, NewF,NewP);
		Data = {circular,X,Y,I,J,F,Dir} ->
			io:format("circle, ~p~n",[Data] ),
			{{Xs,Ys},NewP} = moveFromMode(Position,{X,Y},rel),
			handle_circular(Handles,Xs,Ys,I,J,F,Dir,Position),
			workerLoop(Handles, Feed, NewP);
		{hold} ->
			io:format("hold, ~n",[] ),
			State = handleHold(Handles),
			workerLoop(State, Feed, Position);
		{release} ->
			io:format("release~n"),
			release(Handles),
			workerLoop(Handles, Feed, Position)
	end.

moveFromMode({Xi,Yi},{Xc,Yc},Mode) ->
	case Mode of
		rel ->
			{{Xc,Yc},{Xi+Xc,Yi+Yc}};
		abs -> 
			{{Xc-Xi,Yc-Yi},{Xc,Yc}}
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

handle_linear(Handles,X,Y,F,Mode,Pos) ->
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
			svg ! {segment, Mode, Pos, sign(X), sign(Y), x},
			{A1,A2} = line_to_steps(X,Handles#handles.x,Y,Handles#handles.y,Wait),
			{A1,A2};
		_ when Ya>Xa ->
			io:format("Y > X ~n",[]),
			svg ! {segment, Mode, Pos, sign(X), sign(Y), y},
			{A1,A2} = line_to_steps(Y,Handles#handles.y,X,Handles#handles.x,Wait),
			{A2,A1} % le flip 
	end,
	svg ! {endsegment},
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
		  %S1+1 = what we are stepping to. 
		  % 0.5 to step half way in between, also eliminates need for >=
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
			svg ! {step, 1, 1},
			line_to_steps(A1-1,A1_handles,A1_dir,A2-1,A2_handles,A2_dir,Q,W,S1+1,S2+1);
		false ->
			io:format("Step A1 only~n",[]),
			% wait W
			svg ! {step, 1, 0},
			line_to_steps(A1-1,A1_handles,A1_dir,A2,A2_handles,A2_dir,Q,W,S1+1,S2)
	end.
handle_circular(Handles,Xt,Yt,I,J,F,Dir,AbsPos) ->
	svg ! {segment, tool, AbsPos, 1, 1, x}, % Transperent input
	Sign = dirToRadSign(Dir),
	Steps = myabs(pyth(I,J)*sumAngles(math:atan2(-J,-I), math:atan2(Yt-J,Xt-I), Dir)),
	io:format("Steps: ~p Angles:~p~n",[Steps,sumAngles(math:atan2(-J,-I), math:atan2(Yt-J,Xt-I), Dir)]),
	arc_to_steps(Handles,Steps,math:atan2(-J,-I),{0,0},{Xt,Yt},{I,J},pyth(I,J),F,Sign),
	svg ! {endsegment}.


arc_to_steps(Handles,0,_Angle,_Pos,_TargetPos,_CenterPos,_R,_F,_Sign) ->
	Handles;
arc_to_steps(Handles,Steps,_Angle,_Pos,_TargetPos,_CenterPos,_R,_F,_Sign) when Steps < 1 ->
	io:format("gotcha!",[]),
	Handles;

arc_to_steps(Handles,Steps,Angle,Pos,TargetPos,CenterPos,R,F,Sign) ->
	Anext = Angle+Sign*(1/R),
	{Xnow,Ynow} = Pos,
	{Xcenter,Ycenter} = CenterPos,
	Xnext = math:cos(Anext)*R,
	Ynext = math:sin(Anext)*R,
	Xstep = round((Xcenter + Xnext)-Xnow),
	Ystep = round((Ycenter + Ynext)-Ynow), 
	io:format("Steps: ~p, Angle:~p, Pos:~p, TargetPos:~p, CenterPos~p, R:~p, Sign:~p, Anext:~p, Xnext:~p, Ynext:~p ~n",
		[Steps,Angle,Pos,TargetPos,CenterPos,R,Sign,Anext,Xnext,Ynext]),
	case stepWhat(myabs(Xstep),myabs(Ystep)) of
		both ->
			io:format("Step both", []),
			svg ! {step, Xstep, Ystep};
		x ->
			io:format("Step x", []),
			svg ! {step, Xstep, Ystep};
		y ->
			io:format("Step y", []),
			svg ! {step, Xstep, Ystep};
		neither ->
			io:format("Step neither", [])
			%error("nostep circle")
	end,
	arc_to_steps(Handles,Steps-1,Anext,{Xnow+Xstep,Ynow+Ystep},TargetPos,CenterPos,R,F,Sign).


stepWhat(1,1) ->
	both;
stepWhat(1,0) ->
	x;
stepWhat(0,1) ->
	y;
stepWhat(0,0) ->
	neither.


sumAngles(A1,A2,Dir) ->
	io:format("Sumangles A1:~p, A2:~p, Dir:~p~n",[A1,A2,Dir]),
	case Dir of
		cw ->
			case diffsign(A1,A2) of
				true ->
					case A1 > A2 of
						true ->
							A1+myabs(A2);
						false ->
							math:pi()-myabs(A1)+math:pi()-A2
						end;
				false ->
					case myabs(A1) >= myabs(A2) of
						true ->
							2*math:pi()-(A1-A2);
						false -> 
							A1-A2
					end
			end;
		ccw ->
			case diffsign(A1,A2) of
				true ->
					case A1 > A2 of
						true ->
							math:pi()-A1+math:pi()-myabs(A2);
						false ->
							myabs(A1)+A2
					end;
				false ->
					case myabs(A2) >= myabs(A1) of
						true ->
							2*math:pi()-(A2-A1);
						false -> 
							A2-A1
					end
			end
	end.

diffsign(A1,A2) ->
	not ((A1 >=0) and (A2 >= 0)) or ((A1 <0) and (A2 < 0)).
 
pyth(X,Y) ->
	math:sqrt(math:pow(myabs(X),2)+math:pow(myabs(Y),2)).

dir(Var) ->
	case Var < 0 of
		true ->
			backward;
		false ->
			forward
	end.

sign(Var) ->
	case Var < 0 of
		true ->
			-1;
		false ->
			1
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

