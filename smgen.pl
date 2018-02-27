#!/usr/bin/swipl -s
% Generate matlab/python(python) solver for equations in file "eqn"
% ./smgen.pl eqn

:- use_module(library(lists), [ nth0/3 ]).

suffix(matlab, '.m').
suffix(python, '.py').

gencode(Language, Name, Sign) -->
    { preamble(Pre),
      variation(Var),
      odes(ODEs),
      setof(V,freevars([Pre,Var,ODEs],V),AllVars), % all variables
      setof(Lh, Eq^member(Lh=Eq,ODEs), LHS),       % minus ODE names
      ord_subtract(AllVars, LHS, Vars) },
      declare(Language, Name, Vars),                % Declaration
      findall(S, (member(M,Pre),S=..[Language,M])), % Assignments
      modulation(Var, Language, Sign),              % Variation
      diffeq(Language, ODEs).                       % Equations

genmodels(Language) :-
    gencode(Language, f2, '', PCode, []),
    writecode(Language, f2, PCode),
    gencode(Language, g2, '-', NCode, []),
    writecode(Language, g2, NCode).

flat_format([])        --> !, [].
flat_format([H|T])     --> !, flat_format(H), flat_format(T).
flat_format(matlab(A)) --> !, space, [A, ';'], newline.
flat_format(python(A)) --> !, tab, [A], newline.
flat_format(A) --> [A].

writecode(Language, Name, RawCode) :-
    flat_format(RawCode, Code,[]),
    suffix(Language, Suffix),
    concat_atom([Name, Suffix],Filename),
    tell(Filename),
    maplist(write, Code),
    told.

% +- Modulation of specified variables to model range of values
% E.g. Create local version (lkg) of var (kg) with +- delta
modulation([], _, _)               --> [].
modulation([V|Vs], Language, Sign) -->
    delta(Language, Sign, V), newline,
    modulation(Vs, Language, Sign).

delta(matlab, Sign, V) --> space, delta(Sign, V), [';'].
delta(python, Sign, V) --> tab, delta(Sign, V).
% Language Independent
delta(Sign, VN:PC) --> ['loc',VN,' = delta(',VN,', ',Sign,PC,'*pm)'].


% DIFFERENTIAL EQUATION SYNTAX
diffeq(matlab, Stmts) -->
    findall(['    xdot(',I,') = ', matlab(S)], nth0(I,Stmts,_=S)).

diffeq(python, Stmts) -->   % return [ Expr,<nl> Expr,<nl> ... ]
    ['\t', return, ' [' ], newline,
    { findall(S, member(_=S,Stmts), List) },
    frame_list('    ',List,',\n'),
    [']'], newline.

% DIFFERENTIAL EQUATION SYNTAX

main :- current_prolog_flag(argv,[A|_]),
	consult(A),
	genmodels(matlab),
	genmodels(python).

freevars([],_)  :- !,fail.
freevars(N,_)   :- number(N), !, fail.
freevars(A,B)   :- atomic(A), !, not_local(A,B).
freevars(Term,V):- Term =.. [_|Args],
		   member(A, Args),
		   freevars(A, V).
    
not_local(LVar,Var) :- atom_concat(loc,Var,LVar),!.
not_local(Var, Var).
    
newline --> ['\n'].
space   --> ['    '].
tab     --> ['\t'].

tabs(0) --> [].
tabs(N) --> { N>0, NN is N-1}, tab, tabs(NN).

declare(matlab, Name, List) -->
    ['function xdot = ', Name, '('], xandt, newline,
    [global, ' '], comma_list(List), [';'], newline.

declare(python, Name, List) -->
    [def, ' ', Name, '('], comma_list(List),[','],xandt, [':'], newline.

xandt --> ['x', ',', 't', ')'].

comma_list(List) --> frame_list(' ',List,',').

frame_list(Pre,[H1,H2|T],Post) --> [Pre,H1,Post],!,frame_list(Pre,[H2|T],Post).
frame_list(Pre,L,_) --> [Pre], L.

