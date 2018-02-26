% Create a web page for user access to all (non-local) variables.

sorted_vars(Vars) :-
    consult(eqn),
    preamble(Pre),
    variation(Var),
    odes(ODEs),
    setof(V,freevars([Pre,Var,ODEs],V),AllVars),
    findall(LH, member(LH=_,ODEs), LHS),
    ord_subtract(AllVars, LHS, Vars).
	
freevars([],_)  :- !,fail.
freevars(N,_)   :- number(N), !, fail.
freevars(A,B)   :- atomic(A), !, not_local(A,B).
freevars(Term,V):-
    Term =.. [_|Args],
    member(A,Args),
    freevars(A,V).
    
not_local(LVar,Var) :- atom_concat(loc,Var,LVar),!.
not_local(Var, Var).
