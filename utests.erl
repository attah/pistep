-module(utests).
-export([run/0]).

run() ->
	sumAnglesTest(),
	ok.

cmp10(A,B) ->
	trunc(A*10000000000)==trunc(B*10000000000).
	%{A,B}.

sumAnglesTest() ->
	Da = [  { {1,3},{2,3},ccw, {1,3} },
			{ {1,3},{2,3}, cw, {5,3} },
			{ {2,3},{-2,3},ccw, {2,3} },
			{ {2,3},{-2,3}, cw, {4,3} },
			{ {-2,3},{-1,3},ccw, {1,3} },
			{ {-2,3},{-1,3}, cw, {5,3} },
			{ {-1,3},{1,3},ccw, {2,3} },
			{ {-1,3},{1,3}, cw, {4,3} }
			],
% math:round(RM*math:pi()/RD, 3) ==
	Test = fun(Data) -> {{A1M,A1D},{A2M,A2D}, Dir, {RM,RD}} = Data, cmp10(RM*math:pi()/RD, pistep:sumAngles(A1M*math:pi()/A1D, A2M*math:pi()/A2D, Dir)) end,
	Res = [ Test(D) || D <- Da ],
	io:format("sumAnglesTest gives ~p", [Res]).
