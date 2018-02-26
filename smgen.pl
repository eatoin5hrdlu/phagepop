#!/usr/bin/swipl -s
% Generate octave/python(python) solver for equations in file "eqn"
% ./smgen.pl eqn

:- use_module(library(lists), [ nth0/3 ]).

suffix(octave, '.m').
suffix(matlab, '.m').
suffix(python, '.py').

% Compute +- parameter values for range of solutions
deltaValue(octave, Sign, VN:PC, ['l',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).
deltaValue(python, Sign, VN:PC, ['\tl',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).

gencode(Language, Name, _Spec, _Sign) -->
    declaration(Language, Name),
    assignments(Language),
    diffeq(Language).

genmodels(Language) :-
    Spec = [kg:0.02, ad:0.1, ec:0.1, pp:0.05],
    writecode(Language, Spec, f2, ''),
    writecode(Language, Spec, g2, '-').

flat_format([])        --> [].
flat_format([H|T])     --> flat_format(H), flat_format(T).
flat_format(octave(A)) --> !, [A, ';'], newline.
flat_format(python(Tabs, A)) --> !, tabs(Tabs), [A], newline.
flat_format(A) --> [A].

tabs(N) --> { N>0, !, NN is N-1}, ['\t'], tabs(NN).
tabs(0) --> [].

writecode(Language, Spec, Name, Sign) :-
    gencode(Language, Name, Spec, Sign, Code, []),
    suffix(Language, Suffix),
    concat_atom([Name, Suffix],Filename),
    tell(Filename),
    flat_format(Code, FlatCode,[]),
    maplist(write, FlatCode),
    told.

% ASSIGNMENT STATEMENT SYNTAX
assignments(Language) --> { preamble(Pre) }, stmt(Pre,Language).

stmt([],    _)      --> !.
stmt([H|T], octave) --> [ octave(H) ], stmt(T, octave).
stmt([H|T], python) --> [ python(1, H) ], stmt(T, python).

# DIFFERENTIAL EQUATION SYNTAX
diffeq(octave) --> diffeq(matlab).

diffeq(matlab) -->  { odes(Stmts),
		      findall(['xdot(',I,')= ', octave(S)], nth0(I,Stmts,_=S), ODEs) },
		    ODEs.

diffeq(python) --> ['\t', return, '[' ], newline,
		   { odes(Stmts),
		     findall([S,',\n'], member(_=S,Stmts), ODEs) },
		   ODEs, [']'], newline.

run :- current_prolog_flag(argv,[A|_]),
       consult(A),
       genmodels(octave),
       genmodels(python).

    
freevars([],_)  :- !,fail.
freevars(N,_)   :- number(N),!,fail.
freevars(A,A)   :- atomic(A),!.
freevars(Term,V):- Term =.. [_|Args], member(A,Args), freevars(A,V).
    
declaration(Language, Name) -->
    { preamble(P),
      setof(V,freevars(P,V),Vs) }, 
    declare(Language, Name, Vs).
    
newline --> ['\n'].

declare(octave, Name, List) --> declare(matlab, Name, List).

declare(matlab, Name, List) --> ['function xdot = ', Name, '('], xandt, newline,
                                [global, ' '], dvarlist(matlab, List), [';'], newline.

declare(python, Name, List) --> [def, ' ', Name, '('], dvarlist(python, List), xandt, [':'], newline.

xandt --> ['x', ',', 't', ')'].

dvarlist(_,           []) --> [].
dvarlist(matlab,     [H]) --> [H], !.
dvarlist(Language, [H|T]) --> [H, ', '], dvarlist(Language, T).

