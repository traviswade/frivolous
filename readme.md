Some simple Erlang parse transforms. 
All of the transforms use the functions in the [erl_syntax](http://erlang.org/doc/man/erl_syntax.html)
module rather than pattern matching on syntax trees. This should mean that they will work
in future versions of the language, but you never know.

###Cuts

Works like the most basic of the cuts in [erlando](https://github.com/rabbitmq/erlando),
which were inspired by [scheme](). Turns an application including
one or more `_` arguments into a `fun` that takes the missing items as arguments.

	f(X, _, Y, _).
	
becomes

	fun (Arg1, Arg2) -> f(X, Arg1, Y, Arg2) end.


###Simple Pipes

Overloads `/` to mean function application, just like the `|>` operator that all the other kids have.

	X / g / f
	
becomes

	f(g(X))
	
Atoms, funs and implicit fun expressions are allowed in the function position. Variables and function
applications will also work if wrapped in curly braces.

	Even = lists:filter(fun (X) -> X rem 2 =:= 0 end, _),
	"400"
		/ list_to_integer
		/ {lists:seq(1, _)}
		/ {Even}
		/ fun lists:max/1
		/ fun (_) -> "why would you do that" end.


###Smarter Pipes

For some of the things that you might use monads for in Erlang. 
Use the `+` operator with a chain of piped applications to wrap each one with a function.

`bind + x / h / g / f` becomes

	bind(fun (Arg) -> f(Arg) end,
		bind(fun (Arg) -> g(Arg) end,
			bind(fun (Arg) -> h(Arg) end, X)))

So for example instead of

	case f(X) of
		{ok, ResF} ->
			case g(ResF) of
				{ok, ResG} ->
					case h(ResG) of
						{ok, ResH} -> 
							i(ResH);
						{error, Why} ->
							{error, Why}
					end;
				{error, Why} ->
					{error, Why}
			end;
		{error, Why} ->
			{error Why}
	end
	
you can say
	
	Maybe = fun (F, {ok, Res})    -> F(Res); 
	            (_, {error, Why}) -> {error, Why} end,
	
	{Maybe} + X / i / h / g / f
	
The rules for types in the `bind` position are the same as for pipes.
				
####Skipping a step

Sometimes the output of one of the functions you want to chain (or often
the initial input to the chain) doesn't match the form expected by your bind function.
Just use `*` in place of `/` to skip the function wrapping for that step. 

	maybe + x * g / oddfun * f

becomes
	
	f(maybe(fun (Arg) -> oddfun(Arg) end, g(3)))
	
###Multiple patterns in case clauses

Clause patterns with the 'or' operator are copied into separate clauses:

	case A of
	  x or y or z -> 0;
	  _ -> 1;
	end

becomes

	case A of
	  x -> 0;
	  y -> 0;
	  z -> 0;
	  _ -> 1;
	end


###Use

To get everything, use

	-compile({parse_transform, frivolous})

Cut, pipe and case transforms are run separately, in that order.
	
`frivolous_cut`,  `frivolous_pipe` and `frivolous_case` can be used separately.

To get a pretty print of the transforms that are performed during compilation,
add a {frivolous, [verbose]} compile flag.





