-module(friv_tests).

-compile({parse_transform, frivolous}).
 
f1 (A, B) -> A * B.
f2 ({A,B}, [H|T], X) -> {B, H, [A+X|T]}.

add (A, B) -> A + B.
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

f5 (Val) ->
	Even = (Val rem 2 =:= 0),
	if Even -> {ok, Val+1};
		true -> {error, odd}
	end.
f6 (Val) ->
	maybe > {ok, Val} 
		/ f3
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
	?assertEqual(16, identity > 2 / double / double / double).
	
maybe_test () ->
	?assertEqual({error, lessthan2}, f6(2)),
	?assertEqual({error, hitfive}, f6(4)),
	?assertEqual({error, odd}, f6(6)),
	?assertEqual({ok, 13}, f6(3)).
	
	
	
	