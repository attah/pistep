-module(pistep).
-export([start/0]).
-export([init/0]).
-export([workerInit/2]).
-export([svg_init/0]).
-export([laser_init/0]).
-export([second_timer/0]).
-export([readFile/1]).
-record(g00,{x,y}).
-record(g01,{x,y,f}).
-record(g02,{x,y,i,j,f}).
-record(g03,{x,y,i,j,f}).

setf(R=#g00{},x,X) ->
	R#g00{x=X};
setf(R=#g00{},y,Y) ->
	R#g00{y=Y};

setf(R=#g01{},x,X) ->
	R#g01{x=X};
setf(R=#g01{},y,Y) ->
	R#g01{y=Y};
setf(R=#g01{},f,F) ->
	R#g01{f=F};

setf(R=#g02{},x,X) ->
	R#g02{x=X};
setf(R=#g02{},y,Y) ->
	R#g02{y=Y};
setf(R=#g02{},i,I) ->
	R#g02{i=I};
setf(R=#g02{},j,J) ->
	R#g02{j=J};
setf(R=#g02{},f,F) ->
	R#g02{f=F};

setf(R=#g03{},x,X) ->
	R#g03{x=X};
setf(R=#g03{},y,Y) ->
	R#g03{y=Y};
setf(R=#g03{},i,I) ->
	R#g03{i=I};
setf(R=#g03{},j,J) ->
	R#g03{j=J};
setf(R=#g03{},f,F) ->
	R#g03{f=F};

setf(R,z,_)->
	R.

-record(handles,{x,y}).

-define(RES,40). %steps per mm
%-define(RES,10). %steps per mm

% " style="fill:#ffffff; fill-opacity:0; stroke:#000000; stroke-width:1;"/> </g> </g> </svg>
-define(SvgHead,"<?xml version=\"1.0\" standalone=\"yes\" ?> <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\" \"http://www.w3.org/TR/2001/PR-SVG-20010719/DTD/svg10.dtd\"> <svg version=\"1.1\" baseProfile=\"full\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:ev=\"http://www.w3.org/2001/xml-events\" width=\"2000\" height=\"2500\"> ~s <g transform=\"translate(0,2500)\"><g transform=\"scale(1,-1)\">").
-define(SvgStyle,"<style type=\"text/css\"><![CDATA[path{pointer-events:stroke;fill:none;stroke-width:1;}path.tool{stroke:#111111;}path.move{stroke:#000066;}path:hover{stroke:#660000;} ]]></style>").
-define(SvgEnd,"</g></g></svg>").


% h4xx c(pistep,[{d,debug,1}]).
-ifdef(debug).
gpio_init(What,_Dir) ->
	What.
-else.
gpio_init(What,Dir) ->

	gpio:init(What,Dir).
-endif.


-ifdef(debug).
gpio_write(What,Value) ->
	io:format("Wrote ~p to ~p~n",[Value,What] ).
-else.
gpio_write(What,Value) ->
	gpio:write(What,Value).
-endif.

%handlesToAxis(Handles) ->
%	case lists:member(14,Handles) of
%		true

%make more rubust when gripping

svg_init() ->
	register(svg, self()),
	{ok, TFd} = file:open("time.debug", [write]),
	file:write(TFd,io_lib:format("Start ~p ~n",[time()])),
	case file:open("debug.svg", [write]) of
		{ok, Fd} ->
			file:write(Fd, io_lib:format(?SvgHead,[?SvgStyle])),
			svg_loop(Fd,TFd,0,0);
		{error, Reason} ->
			Reason
	end.

svg_loop(Fd,TFd, Xacc, Yacc) ->
	receive 
		{segment, M, {Xi,Yi}, Xsign, Ysign, A1} ->
			file:write(Fd, io_lib:format("<path class=\"~p\" d=\"M~p, ~p ", [M,Xi,Yi])),
			{Xp,Yp} = inner_svg_loop(Fd, Xsign, Ysign, A1, Xacc, Yacc),
			file:write(Fd, io_lib:format("\" />",[])),
			svg_loop(Fd,TFd, Xacc+Xp, Yacc+Yp);
		{endprogram} ->
			%io:format("Sum ~p ~n", [{Xacc,Yacc}]),
			file:write(TFd,io_lib:format("End ~p ~n",[time()])),
			file:close(TFd),
			file:write(Fd, ?SvgEnd),
			file:close(Fd)
	end.


inner_svg_loop(Fd, Xsign, Ysign, PrimaryAxis, Xacc, Yacc) ->
	receive 
		{step, A1, A2} ->
			{X,Y} = case PrimaryAxis of
				x -> {Xsign*A1,Ysign*A2};
				y -> {Xsign*A2,Ysign*A1}
			end,
			file:write(Fd, io_lib:format(" l ~p ~p ",[X,Y])),
			%io:format("Krafs! ~p, ~p, ~p ~n", [{X,Y},{Xacc,Yacc},{Xacc+X, Yacc+Y}]),
			inner_svg_loop(Fd, Xsign, Ysign, PrimaryAxis, Xacc+X, Yacc+Y);
		{endsegment} ->
			%io:format("SegSum ~p ~n", [{Xacc,Yacc}]),
			{Xacc,Yacc}
	end.

start() ->

	spawn_link(?MODULE, svg_init, []),
	spawn_link(?MODULE, laser_init, []),
	spawn_link(?MODULE, init, []).

	%loop(0, List).

init() ->
	register(gsrv, self()),
	Worker = spawn_link(?MODULE, workerInit, [[14, 15, 17, 18],[25, 24, 23, 22]]),
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
				after 1000 ->
					exit("herp")
			end
	end,
	gsrv(Worker).

handle_g00(Cmd,Worker) ->
	Worker ! {linear,
			  Cmd#g00.x*?RES,
			  Cmd#g00.y*?RES,
			  maximum,
			  move}.

handle_g01(Cmd,Worker) ->
	Worker ! {linear,
			  Cmd#g01.x*?RES,
			  Cmd#g01.y*?RES,
			  Cmd#g01.f,
			  tool}.

handle_g02(Cmd,Worker) ->
	Worker ! {circular,
			  Cmd#g02.x*?RES,
			  Cmd#g02.y*?RES,
			  Cmd#g02.i*?RES,
			  Cmd#g02.j*?RES,
			  Cmd#g02.f,
			  cw}.

handle_g03(Cmd,Worker) ->
	Worker ! {circular,
			  Cmd#g03.x*?RES,
			  Cmd#g03.y*?RES,
			  Cmd#g03.i*?RES,
			  Cmd#g03.j*?RES,
			  Cmd#g03.f,
			  ccw}.


workerInit(Xlist,Ylist) ->
	X = [ gpio_init(P,out) || P <- Xlist ],
	Y = [ gpio_init(P,out) || P <- Ylist],

	workerLoop(#handles{x=X,y=Y}, 0.5, {0,0}).

workerLoop(Handles, Feed, Position) ->
	io:format("worker ready~n"),
	gsrv ! {done,self()},
	release(Handles),
	receive
		Data = {linear,X,Y,F,Mode} ->
			io:format("linear, ~p~n",[Data] ),
			%XXX use saved feed if undef!
			NewF = whichF(Feed,F,Mode),
			{{Xs,Ys},NewP} = moveFromMode(Position,{X,Y},abs),
			State = handle_linear(Handles,Xs,Ys,NewF,Mode,Position),
			workerLoop(State, NewF,NewP);
		Data = {circular,X,Y,I,J,F,Dir} ->
			io:format("circle, ~p~n",[Data] ),
			NewF = whichF(Feed,F,tool),
			io:format("THAT F ~p ~n",[NewF]),
			{{Xs,Ys},NewP} = moveFromMode(Position,{X,Y},abs),
			%{{Is,Js},_} = moveFromMode(Position,{I,J},abs),
			Is = I,
			Js = J,
			io:format("arcing from ~p to ~p by ~p with center ~p ~n",[Position,NewP,{Xs,Ys},{Is,Js}]),
			%handle_circular(Handles,Xs,Ys,I,J,NewF,Dir,Position), FIXME
			handle_circular(Handles,round(Xs),round(Ys),round(Is),round(Js),NewF,Dir,Position),
			workerLoop(Handles, NewF, NewP);
		{hold} ->
			io:format("hold, ~n",[] ),
			Handles = handleHold(Handles), % Check that handles were returned in the same position
			workerLoop(Handles, Feed, Position);
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
	case F_new of
		undefined ->
		 	F_old;
		 _ ->
			case Mode of
				tool ->
					F_new;
				move ->
					F_old
			end
	end.

release({handles,X,Y}) ->
	release(X),
	release(Y);
release([A,B,C,D]) ->
	wait_ms(1),
	gpio_write(A,0),
	wait_ms(1),
	gpio_write(B,0),
	wait_ms(1),
	gpio_write(C,0),
	wait_ms(1),
	gpio_write(D,0),
	wait_ms(1).

grip({handles,X,Y}) ->
	grip(X),
	grip(Y);
grip([A,B,C,D]) ->
	wait_ms(1),
	gpio_write(D,0),
	wait_ms(1),
	gpio_write(C,0),
	wait_ms(1),
	gpio_write(B,1),
	wait_ms(1),
	gpio_write(A,1),
	wait_ms(1).


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
	gpio_write(B,1),
	gpio_write(A,1),
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

	laserOn(Mode), % note 100 ms wait in laser handler when going from tool to move

	{Xhandles,Yhandles} = case Xa of
		%Y ->
		%	io:format("X == Y",[]);
		_ when Xa>=Ya ->
			io:format("X > Y ~n",[]),
			svg ! {segment, Mode, Pos, sign(X), sign(Y), x},
			{A1,A2} = line_to_steps(trunc(X),Handles#handles.x,trunc(Y),Handles#handles.y,Wait),
			{A1,A2};
		_ when Ya>Xa ->
			io:format("Y > X ~n",[]),
			svg ! {segment, Mode, Pos, sign(X), sign(Y), y},
			{A1,A2} = line_to_steps(trunc(Y),Handles#handles.y,trunc(X),Handles#handles.x,Wait),
			{A2,A1} % le flip 
	end,
	laserOff(), 
	svg ! {endsegment},
	#handles{x=Xhandles,y=Yhandles}.

line_to_steps(0,A1_handles,0,A2_handles,_W) ->
	{A1_handles,A2_handles};

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
					%io:format("Step A2~n",[]);
					ok;
				false ->
					io:format("WARNING we have run out of A2 steps, but more are expected.. A1=~p A2=~p S=~p Q=~p~n",[A1,A2,S1,Q]),
					exit("wtf")
			end,
			%io:format("Step A1 after A2~n",[]),
			% wait sqrt(2)*W
			svg ! {step, 1, 1},
			{handles,A1state,A2state} = stepAccordingly({handles,A1_handles,A2_handles},1,A1_dir,1,A2_dir,W),
			line_to_steps(A1-1,A1state,A1_dir,A2-1,A2state,A2_dir,Q,W,S1+1,S2+1);
		false ->
			%io:format("Step A1 only~n",[]),
			% wait W
			svg ! {step, 1, 0},
			{handles,A1state,A2state} = stepAccordingly({handles,A1_handles,A2_handles},1,A1_dir,0,A2_dir,W),
			line_to_steps(A1-1,A1state,A1_dir,A2,A2state,A2_dir,Q,W,S1+1,S2)
	end.
handle_circular(Handles,Xt,Yt,I,J,F,Dir,AbsPos) ->
	svg ! {segment, tool, AbsPos, 1, 1, x}, % Transperent input
	laserOn(tool),
	Sign = dirToRadSign(Dir),
	Steps = myabs(pyth(I,J)*sumAngles(math:atan2(-J,-I), math:atan2(Yt-J,Xt-I), Dir)),
	io:format("Steps: ~p Angles:~p~n",[Steps,sumAngles(math:atan2(-J,-I), math:atan2(Yt-J,Xt-I), Dir)]),
	arc_to_steps(Handles,Steps,math:atan2(-J,-I),{0,0},{Xt,Yt},{I,J},pyth(I,J),F,Sign),
	laserOff(),
	svg ! {endsegment}.


arc_to_steps(Handles,0,_Angle,_Pos,_TargetPos,_CenterPos,_R,_F,_Sign) ->
	io:format("Proper circle completion ~n",[]),
	Handles;
arc_to_steps(Handles,Steps,_Angle,Pos,Pos,_CenterPos,_R,_F,_Sign)  when Steps =< 1 ->
	io:format("Proper circle completion 2 ~n",[]),
	Handles;
arc_to_steps(Handles,Steps,_Angle,Pos,TargetPos,_CenterPos,_R,F,_Sign) when Steps =< 0 ->
	io:format("gotcha!",[]),
	{Xnow,Ynow} = Pos,
	{Xt,Yt} = TargetPos,
	Xd = Xt - Xnow,
	Yd = Yt - Ynow,
	Xda = myabs(Xd),
	Yda = myabs(Yd),
	case Xda+Yda of
		S when S > 2; Xda =:= 2; Yda =:= 2 ->
			error(notRound);
		_S ->
			io:format("closing circle segment~p ~p ~n", [Xd,Yd]),
			svg ! {step, Xd, Yd},
			stepAccordingly(Handles,Xd,Yd,feedToWait(F,tool)) % proper return value
	end;

arc_to_steps(Handles,Steps,Angle,Pos,TargetPos,CenterPos,R,F,Sign) ->
	Anext = Angle+Sign*(1/R),

	{Xnow,Ynow} = Pos,
	{Xcenter,Ycenter} = CenterPos,

	Xnext = math:cos(Anext)*R,
	Ynext = math:sin(Anext)*R,
	Xstep = round((Xcenter + Xnext)-Xnow),
	Ystep = round((Ycenter + Ynext)-Ynow), 

	%% peek ahead and double step maybe...

	%io:format("Steps: ~p, Angle:~p, Pos:~p, TargetPos:~p, CenterPos~p, R:~p, Sign:~p, Anext:~p, Xnext:~p, Ynext:~p Action:~p ~n",
	%	[Steps,Angle,Pos,TargetPos,CenterPos,R,Sign,Anext,Xnext,Ynext,{Xstep,Ystep}]),

	svg ! {step, Xstep, Ystep},
	NewHandles = stepAccordingly(Handles,Xstep,Ystep,feedToWait(F,tool)),

	arc_to_steps(NewHandles,Steps-1,Anext,{Xnow+Xstep,Ynow+Ystep},TargetPos,CenterPos,R,F,Sign).


stepAccordingly(Handles,A1,A2,W) ->
	stepAccordingly(Handles,myabs(A1),dir(A1),myabs(A2),dir(A2),W).

stepAccordingly(Handles={handles,A1handles,A2handles},A1,A1dir,A2,A2dir,W) ->
	A1state = case A1 of
		0 ->
			A1handles;
		1 ->
			[H11,H12,H13,H14] = shiftAccordingly(A1handles, A1dir),
			gpio:write(H14,0),
			gpio:write(H13,0),
			gpio:write(H11,1),
			gpio:write(H12,1),
			[H11,H12,H13,H14]
	end,
	A2state = case A2 of
		0 ->
			A2handles;
		1 ->
			[H21,H22,H23,H24] = shiftAccordingly(A2handles,A2dir),
			gpio:write(H24,0),
			gpio:write(H23,0),
			gpio:write(H21,1),
			gpio:write(H22,1),
			[H21,H22,H23,H24] 
	end,
	Wait = case A1+A2 of
		2 ->
			round(1.4*W);
		1 ->
			W;
		0->
			0
	end,
	wait_ms(Wait),
	laser ! {check,Handles,self()},
	receive
		{ok} ->
		 	ok
	end,
	{handles,A1state,A2state}.

-ifdef(debug).
wait_ms(_W) ->
	ok.
-else.
wait_ms(W) ->
	receive
		after W ->
			ok
	end.
-endif.

sumAngles(A1,A2,Dir) ->
	A = sumAngles2(A1,A2,Dir),
	io:format("sumangles gives:~p ~n ", [A]),
	A.

sumAngles2(A1,A2,Dir) ->
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
							A1-A2;
						false -> 
							2*math:pi()-(A1-A2)
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
							A2-A1;
						false -> 
							2*math:pi()-(A1-A2)
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

readFile(Name) ->
	{ok, Fd} = file:open(Name,[read]),
	readFileLoop(Fd).

readFileLoop(Fd) ->
	case file:read_line(Fd) of
		{ok, Line} ->
			case Line of
				[$\n | _ ] ->
					io:format("empty line ~n", []);
				[$( | _ ] ->
					io:format("paranthesis line ~n", []);
				[$% | _ ] ->
					io:format("comment line ~n", []);
				LikelyCode ->
					parseCommand(LikelyCode)
			end,
			readFileLoop(Fd);
		eof ->
			ok
	end.

parseCommand(Cmd) ->
	case Cmd of 
		[$M | _ ] ->
			parseMCommand(Cmd);
		[$G | _ ] ->
			parseGCommand(Cmd);
		_ ->
			io:format("ERROR: unknown command: ~p ~n", [Cmd])
	end.

parseMCommand(Cmd) ->
	io:format("M-command: ~p ~n", [Cmd]).

parseGCommand(Cmd) ->
	% Perhaps ensure spaces to split on before cmd fields here...
	case Cmd of
		"G00" ++ _  ->
			%parseG00(string:substr(Cmd,4));
			io:format("G00-command: ~p ~n", [Cmd]),
			fancyGVars(string:substr(Cmd,4),record_info(fields,g00),#g00{});
		"G01" ++ _  ->
			io:format("G01-command: ~p ~n", [Cmd]),
			fancyGVars(string:substr(Cmd,4),record_info(fields,g01),#g01{});
		"G02" ++ _  ->
			io:format("G02-command: ~p ~n", [Cmd]),
			fancyGVars(string:substr(Cmd,4),record_info(fields,g02),#g02{});
		"G03" ++ _  ->
			io:format("G03-command: ~p ~n", [Cmd]),
			fancyGVars(string:substr(Cmd,4),record_info(fields,g03),#g03{});
		_ ->
			io:format("ERROR: unknown G-command: ~p ~n", [Cmd])
	end.

fancyGVars(S,Fields,R)->
	fancyGVars2(string:tokens(cleanupEnd(S)," "),Fields,R).

fancyGVars2([],_Fields,Rec) ->
	io:format("loldafuq ~p~n",[Rec]),
	Rec;
fancyGVars2([H|R],Fields,Rec) ->
	case list_to_atom(string:to_lower(string:substr(H,1,1))) of 
		Type -> %when lists:member(Type,Fields) ->
			fancyGVars2(R,Fields,setf(Rec,Type,l2f(string:substr(H,2))))%;
		%_ ->
		%	error(derp),
		%	derp
	end.

l2f(S) ->
	case string:str(S,".") of
		0 ->
			list_to_integer(S);
		_ ->
			list_to_float(S)
	end.

cleanupEnd(S) ->
	string:substr(S,1,string:cspan(S,"%(\n")).


laserOn(Mode) ->
	case Mode of
		move ->
			laser ! {off,self()};
		tool ->
			laser ! {on, self()}
	end,
	receive
		{ok} ->
			ok
	end.

laserOff() ->
	laser ! {off,self()},
	receive
		{ok} ->
			ok
	end.


laser_init() ->
	Pid = gpio_init(8,out),
	register(laser,self()),
	spawn_link(?MODULE,second_timer,[]),
	laser_loop(Pid).

laser_loop(Pid) ->
	laser_loop(Pid,0,off).

laser_loop(Pid,T,State) when T > 70 ->
	gpio_write(Pid,0),
	io:format("laserTMO fail, ~p~n",[State]),
	receive
		after 100 ->
			ok
	end,
	error(lasertimeout);

laser_loop(Pid,T,State) ->
	receive
		{check,Handles,Client} ->
			%%% XXX assert on!
			case T >= 60 of
				true ->
					gpio_write(Pid,0),
					io:format("MEGAWAIT!",[]),
					release(Handles),
					waitTicks(30),
					grip(Handles),
					receive
					after 5 ->
						ok
					end,
					gpio_write(Pid,1),
					Client ! {ok},
					laser_loop(Pid,0,State);
				false ->
					Client ! {ok},
					laser_loop(Pid,T,State)
			end;
		{on,Client} ->
			gpio_write(Pid,1),
			io:format("ON~n",[]),
			Client ! {ok},
			laser_loop(Pid,T,on);
		{off,Client} ->
			Client ! {ok},
			receive
				Msg = {on,Client} ->
					io:format("SEQSAVE~n",[]),
					self() ! Msg,
					laser_loop(Pid,T,on)
			after 100 ->
				%case State of
				%	off ->
				%		ok;
				%	on ->
				%		gsrv ! derp
				%end,
				gpio_write(Pid,0),
				io:format("OFF~n",[]),
				laser_loop(Pid,T,off)
			end;
		{tick} ->
			io:format("tick!"),
			case State of
				on ->
					laser_loop(Pid,T+1,State);
				off ->
					laser_loop(Pid,T,State)
			end
			
	end.

-ifdef(debug).
waitTicks(_) ->
	ok.
-else.
waitTicks(0) ->
	ok;
waitTicks(N) ->
	receive
		{tick} ->
			io:format("Chill!~n", []),
			waitTicks(N-1)
	end.
-endif.



second_timer() ->
	receive
	after 1000 ->
		laser ! {tick}
	end,
	second_timer().











