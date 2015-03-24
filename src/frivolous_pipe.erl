-module(frivolous_pipe).

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

transform (infix_expr, Node, Opts) ->
	case erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)) of
		'/' -> maybe_do_application(Node, Opts);
		_   -> Node
	end;

transform (_, Node, _)    -> Node.

% % node is '/' operator
maybe_do_application(Node, Opts) ->
	[Val, _Pipe, F] = lists:append(erl_syntax:subtrees(Node)),
	case erl_syntax:type(F) of
		atom          ->  do_application(Node, F, Val, Opts);
		fun_expr      ->  do_application(Node, F, Val, Opts);	
		implicit_fun  ->  do_application(Node, F, Val, Opts);		
		Other         ->  
			io:format("not transforming: ~p~n", [Other]),
			Node
	end.
	
do_application (OrigNode, F, Val, Opts) ->
	frivolous:show_transform(OrigNode, erl_syntax:application(F, [Val]), Opts).
	

