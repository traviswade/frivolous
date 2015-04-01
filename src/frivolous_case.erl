-module(frivolous_case).

-export([parse_transform/2]).
-import(erl_syntax, [type/1]).


parse_transform (Forms, Opts) ->
	erl_syntax:revert_forms(do_transforms(erl_syntax:form_list(Forms), Opts)).
	
do_transforms (Tree, Opts) ->
	transform(case erl_syntax:subtrees(Tree) of
		[] -> Tree;
		L ->
			Subtrees = [[do_transforms(T, Opts) || T<-Group] || Group<-L],
			erl_syntax:update_tree(Tree, Subtrees)
	end, Opts).
	
transform (Node, Opts) -> transform(type(Node), Node, Opts).
transform (case_expr, Node, Opts) -> 
	Clauses = erl_syntax:case_expr_clauses(Node),
	Clauses1 = proc_clauses(Clauses),
	Out = erl_syntax:case_expr(erl_syntax:case_expr_argument(Node), Clauses1),
	case length(Clauses) =:= length(Clauses1) of
		true -> Out;
		_ -> frivolous:show_transform(Node, Out, Opts)
	end;
transform (_, Node, _) -> Node.

proc_clauses (Clauses) -> proc_clauses(Clauses, []).
proc_clauses ([], Acc) -> 
	lists:append(lists:reverse(Acc));
proc_clauses ([Clause|T], Acc) ->
	proc_clauses(T, [check_nested_or(Clause)|Acc]).
	
is_or (Node) ->
	case type(Node) of
		infix_expr -> 
			[_, Op, _] = lists:append(erl_syntax:subtrees(Node)),
			erl_syntax:operator_name(Op) =:= 'or';
		_ -> false
	end.
	
check_nested_or (Clause) ->
	[Ptn] = erl_syntax:clause_patterns(Clause),
	case is_or(Ptn) of
		true ->
			Guard = erl_syntax:clause_guard(Clause),
			Body = erl_syntax:clause_body(Clause),
			[erl_syntax:clause([C], Guard, Body) || C <- make_patterns(Ptn)];
		_    -> 
			[Clause]
	end.
	
make_patterns (Ptn) -> make_patterns(Ptn, []).
make_patterns (Ptn, Acc) ->
	case is_or(Ptn) of
		true ->
			[Pre, _Or, Post] = lists:append(erl_syntax:subtrees(Ptn)),
			make_patterns(Pre, [Post|Acc]);
		_ -> [Ptn|Acc]
	end.
	
	
