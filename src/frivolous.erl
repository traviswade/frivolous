-module(frivolous).

-export([parse_transform/2, show_transform/3]).

parse_transform (Forms, Opts) ->
	frivolous_case:parse_transform(
		frivolous_pipe:parse_transform(
			frivolous_cut:parse_transform(Forms, Opts), Opts), Opts).

get_opts (Raw) ->
	case proplists:get_value(frivolous, Raw) of
		Opts when is_list(Opts) -> Opts;
		_ -> []
	end.

show_transform (From, To, Opts) ->
	case proplists:get_value(verbose, get_opts(Opts)) of
		true -> 
			io:format("~n --frivolously transforming ~n~s~n  to: ~n~s~n~n", 
				[erl_prettypr:format(From), erl_prettypr:format(To)]);
		_ -> ok
	end,
	To.