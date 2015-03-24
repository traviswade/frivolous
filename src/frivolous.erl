-module(frivolous).

-export([parse_transform/2, show_transform/3]).

parse_transform (Forms, Opts) ->
	frivolous_pipe:parse_transform(
		frivolous_cut:parse_transform(Forms, Opts), Opts).
	% frivolous_pipe:parse_transform(frivolous_cut:parse_transform(Forms, Opts), Opts).
	

show_transform (From, To, Opts) ->
	io:format("~n --frivolously transforming ~n~p~n  to: ~n~p~n~n", 
		[erl_prettypr:format(From), erl_prettypr:format(To)]),
	To.