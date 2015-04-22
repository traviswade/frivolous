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
		'/' -> maybe_do_application(Node, Opts, run);
		'*' -> maybe_do_application(Node, Opts, skip);
		'+' -> maybe_do_bind(Node, Opts);
		_   -> Node
	end;

transform (_, Node, _)    -> Node.


% basic pipe will get transformed to application first. mark them for re-processing
% as funs if we do a bind on the way back up. 

maybe_do_application (Node, Opts, Annotation) ->
	[Val, _Op, F] = lists:append(erl_syntax:subtrees(Node)),
	DoApplication = fun (ToApply) ->
		Node1 = erl_syntax:add_ann({pipe, Annotation}, erl_syntax:application(ToApply, [Val])),
		frivolous:show_transform(Node, Node1, Opts) end,
	case type(F) of
		atom             ->  DoApplication(F);
		fun_expr         ->  DoApplication(F);	
		implicit_fun     ->  DoApplication(F);
		module_qualifier ->  DoApplication(F);
		tuple            ->  
			case check_tuple(F) of
				{ok, El} -> DoApplication(El);
				_        -> Node
			end;
		Other            ->  
			Node
	end.
	
%%%%%%%%%%% bind
maybe_do_bind (Node, Opts) ->
	[L, _Bind, R] = lists:append(erl_syntax:subtrees(Node)),
	
	
	case {type(L), type(R)} of
		{atom, application}             ->  process_bind(Node, L, R, Opts);
		{fun_expr, application}         ->  process_bind(Node, L, R, Opts);
		{implicit_fun, application}     ->  process_bind(Node, L, R, Opts);
		{module_qualifier, application} ->  process_bind(Node, L, R, Opts);
		{tuple, application}            ->  
			case check_tuple(L) of
				{ok, El} -> process_bind(Node, El, R, Opts);
				_        -> Node
			end;
		_             ->  Node
	end.
	
process_bind (OrigNode, L, R, Opts) ->
	frivolous:show_transform(OrigNode, do_bind(L, R), Opts).
	
do_bind (Binder, Application) ->
	Op = erl_syntax:application_operator(Application),
	[Arg] = erl_syntax:application_arguments(Application),
	Arg1 = case type(Arg) of
		application -> do_bind(Binder, Arg);
		_           -> Arg
	end,
	case check_annotation(Application) of
		run  -> do_bind(Binder, Op, Arg1);
		skip -> erl_syntax:application(Op, [Arg1]);
		_    -> 
			% no annotation available. we must be at the end. 
			% for example we could have passed an application into the function
			Application
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
		
check_annotation (Node) ->
	try proplists:get_value(pipe, erl_syntax:get_ann(Node)) of
		Val -> Val
	catch _:_ -> err
	end.
	
check_tuple (Node) ->
	case erl_syntax:tuple_elements(Node) of
		[El] ->
			case type(El) of
				variable    -> {ok, El};
				application -> {ok, El};
				_           -> {error, cant_apply}
			end;
		_ -> {error, badlength}
	end.
	
	
	
	
	
	
		

	

