-module(frivolous_cut).

-export([parse_transform/2]).

parse_transform (Forms, Opts) ->
	erl_syntax:revert_forms(do_transforms(erl_syntax:form_list(Forms), Opts)).
	
do_transforms (Tree, Opts) ->
	transform(case erl_syntax:subtrees(Tree) of
		[] -> Tree;
		L ->
			Subtrees = [[do_transforms(T, Opts) || T<-Group] || Group<-L],
			erl_syntax:update_tree(Tree, Subtrees)
	end, Opts).
	
transform (Node, Opts) -> transform(erl_syntax:type(Node), Node, Opts).

transform (application, Node, Opts) -> 
	case lists:any(is(underscore), erl_syntax:application_arguments(Node)) of
		true -> process_cut(Node, Opts);
		_    -> Node
	end;
transform (_, Node, _) -> Node.

% Take an application node and turn it into a fun_expr taking
% the underscores as arguments filling them into the original application
% e.g. a(1, _) becomes fun (Arg1) -> a(1, Arg1) end.
process_cut (Node, Opts) -> 
	{Patterns, Args} = lists:foldl(
		fun (Tree, {AccPatterns, AccArgs}) ->
			case erl_syntax:type(Tree) of
				underscore -> 
					Var = erl_syntax:variable("Arg"++integer_to_list(length(AccArgs))),
					{[Var|AccPatterns], [Var|AccArgs]};
				_  -> 
					{AccPatterns, [Tree|AccArgs]}
			end
		end, {[], []}, erl_syntax:application_arguments(Node)),
	Application = erl_syntax:application(erl_syntax:application_operator(Node), lists:reverse(Args)),
	Clause = erl_syntax:clause(lists:reverse(Patterns), none, [Application]),
	frivolous:show_transform(Node, erl_syntax:fun_expr([Clause]), Opts).
	

is (Type) -> fun (Tree) -> erl_syntax:type(Tree) =:= Type end.
