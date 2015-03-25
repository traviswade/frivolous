-module(frivolous_pipe).

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

transform (infix_expr, Node, Opts) ->
	case erl_syntax:operator_name(erl_syntax:infix_expr_operator(Node)) of
		'/' -> maybe_do_application(Node, Opts);
		'>' -> maybe_do_bind(Node, Opts);
		_   -> Node
	end;

transform (_, Node, _)    -> Node.


% basic pipe will get transformed to application first. mark them for re-processing
% as funs if we do a bind on the way back up. 
% reconfigure to add more options, for example
% connector > val * f1 / f2 * f3 / f4, 
% to skip the connector on f1 and f3.
% Probably just store what to do with the connector right in the annotation

maybe_do_application (Node, Opts) ->
	[Val, _Pipe, F] = lists:append(erl_syntax:subtrees(Node)),
	case type(F) of
		atom          ->  do_application(Node, F, Val, Opts);
		fun_expr      ->  do_application(Node, F, Val, Opts);	
		implicit_fun  ->  do_application(Node, F, Val, Opts);
		% tuple         -> TODO   		
		_             ->  Node
	end.
	
do_application (OrigNode, F, Val, Opts) ->
	frivolous:show_transform(OrigNode, 
		erl_syntax:add_ann(piped, erl_syntax:application(F, [Val])), Opts).
		
%%%%%%%%%%% bind
maybe_do_bind (Node, Opts) ->
	[L, _Bind, R] = lists:append(erl_syntax:subtrees(Node)),
	case {type(L), type(R)} of
		{atom, application}          ->  process_bind(Node, L, R, Opts);
		{fun_expr, application}      ->  process_bind(Node, L, R, Opts);
		{implicit_fun, application}  ->  process_bind(Node, L, R, Opts);
		% tuple
		_             ->  Node
	end.
	
process_bind (OrigNode, L, R, Opts) ->
	frivolous:show_transform(OrigNode, do_bind(L, R), Opts).
	
do_bind (Binder, Application) ->
	Op = erl_syntax:application_operator(Application),
	% fail if more than one argument. what sense would that make?
	[Arg] = erl_syntax:application_arguments(Application),
	case {type(Arg), erl_syntax:get_ann(Application)} of
		{application, [piped]} -> 
			do_bind(Binder, Op, do_bind(Binder, Arg));
		_ -> 
			do_bind(Binder, Op, Arg)
	end.

do_bind (Binder, Operator, Arg) ->
	erl_syntax:application(Binder, [wrap_op(Operator), Arg]).

wrap_op (Op) ->
	case type(Op) of
		fun_expr     -> Op;
		explicit_fun -> Op;
		_ -> 
			Arg = erl_syntax:variable("Arg"),
			Application = erl_syntax:application(Op, [Arg]),
			erl_syntax:fun_expr([erl_syntax:clause([Arg], none, [Application])])
	end.
		
	
	
	
	
	
	
		

	

