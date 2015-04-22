-module(friv_tests).

-export([f3/1, maybe/2]).

-compile({parse_transform, frivolous}).
 
f1 (A, B) -> A * B.
f2 ({A,B}, [H|T], X) -> {B, H, [A+X|T]}.

add (A, B) -> A + B.
add (Val) -> fun (Val1) -> Val + Val1 end.
double (Val) -> Val * 2.

identity (F, Arg) -> F(Arg).
maybe (F, {ok, Val})       -> F(Val);
maybe (_, {error, Reason}) -> {error, Reason}.

f3 (Val) ->
	if Val > 2 -> {ok, Val+1};
		true   -> {error, lessthan2}
	end.
	
f4 (5)   -> {error, hitfive};
f4 (Val) -> {ok, Val*3}.

f4b (5) -> error;
f4b (Val) -> Val * 3.

f5 (Val) ->
	Even = (Val rem 2 =:= 0),
	if Even -> {ok, Val+1};
		true -> {error, odd}
	end.
f6 (Val) -> maybe + Val * f3 / f4 / f5.
f6b (Val) -> friv_tests:maybe + {ok, Val} 
	/ friv_tests:f3 
	/ f4 
	/ f5.


-include_lib("eunit/include/eunit.hrl").
 
cut_test () -> 
	?assertEqual(12, f1(3, 4)),
	?assertEqual(12, (f1(3, _))(4)),
	?assertEqual(12, (f1(_, 3))(4)),
	?assertEqual({2, h, [11, 1]}, f2({1, 2}, [h,1], 10)),
	?assertEqual({2, h, [11, 1]}, (f2(_, _, 10))({1, 2}, [h, 1])),
	?assertEqual({2, h, [11, 1]}, (f2(_, [h,1], _))({1, 2}, 10)).
	
pipe_test () -> 
	?assertEqual(1, 1 / fun(A)->A end),
	?assertEqual(25, 10 / double / add(5,_)),
	?assertEqual(3, [1, 2, 3] / (fun lists:max/1)),
	?assertEqual(3.0, 6/2).
	
smart_pipe_test () ->
	?assertEqual(16, identity + 2 / double / double / double),
	?assertEqual(16, identity + 2 / double * double * double).
	
maybe_test () ->
	?assertEqual({error, lessthan2}, f6(2)),
	?assertEqual({error, hitfive}, f6(4)),
	?assertEqual({error, odd}, f6(6)),
	?assertEqual({ok, 13}, f6(3)),
	?assertEqual({ok, 13}, f6b(3)).
	
tuple_pipe_test () ->
	F = fun (Val) -> Val + 1 end,
	?assertEqual(5, 4 / {F}),
	?assertEqual(12, 4 / {add(2)} / double),
	Skimmer = fun (Fun, Val) -> Fun(Val-1) end,
	?assertEqual(14, {Skimmer} + 5 / double / double),
	?assertEqual(16, {Skimmer} + 5 / double * double),
	Bad = 1,
	?assertError(_, {Bad} + 5 / double / double).
	
doc_test () ->
	?assertEqual({ok, 13}, maybe + 3 * f3 / f4b * f5).

case_test () ->
	A = 10,
	?assertEqual(a, case A+1 of
		9 or 10 or 11 -> a;
		_ -> b
	end),
	?assertEqual(b, case A+2 of
		9 or 10 or 11 -> a;
		12 -> b;
		_ -> c
	end),
	?assertEqual(b, case A+1 of
		9 -> a;
		10 -> a;
		_ -> b
	end),
	?assertEqual({ok, 4}, case [4, 5] of
		{num, N} or [N|_] 
			when is_integer(N) -> {ok, N};
		_ -> {error, notint}
	end).
	
nother_pipe_test () ->
	[1, 2, 3] / lists:max.
	
complex_bind_test () ->
	?assertEqual(12, identity + double(5) / {add(2)}).
	
	
	
	