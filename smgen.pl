#!/usr/bin/swipl -s
% Generate octave/python(odespy) solver for equations in file "eqn"
% ./smgen.pl eqn

:- use_module(library(lists), [ nth0/3 ]).

suffix(octave, '.m').
suffix(odespy, '.py').

% Compute +- parameter values for range of solutions
deltaValue(octave, Sign, VN:PC, ['l',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).
deltaValue(odespy, Sign, VN:PC, ['\tl',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).

% Code Wrapper Fragments:  Pre [Assignments] Mid [Equations] Post
template(octave, Name, ['function xdot = ', Name, '(x,t)
  global kg ad ec pp fr h0 vol;
  pm = 1;'],
 [ 'inhibit = 1.1;
  kgp = lkg*inhibit;
  kgpp = kgp*inhibit;','\n'],
 ['endfunction','\n']).

template(odespy, Name, ['def ',Name,
 '(x,t)\n\tglobal kg ad ec pp fr h0 vol;\n\tpm = 1;\n'],
 [ '\tinhibit = 1.1;\n\tkgp = lkg*inhibit;\n\tkgpp = kgp*inhibit;\n'],
 ['\n']).

gencode(Language, Name, Spec, Sign,
[ Pre, '\n',
  Assignments, '\n',
  Mid, '\n',
  Equations,'\n',
  Post ]) :-
    template(Language, Name, Pre, Mid, Post),
    maplist(deltaValue(Language, Sign), Spec, Assignments),
    diffeq(Language, Equations).

genmodels(Language) :-
    Spec = [kg:0.02, ad:0.1, ec:0.1, pp:0.05],
    writecode(Language, Spec, f2, ''),
    writecode(Language, Spec, g2, '-').

flat_format([])    --> [].
flat_format([H|T]) --> flat_format(H), flat_format(T).
flat_format(octave(A))       --> !, [A, ';\n'].
flat_format(odespy(Tabs, A)) --> !, tabs(Tabs), [A, '\n'].
flat_format(A) --> [A].

tabs(N) --> { N>0, !, NN is N-1}, ['\t'], tabs(NN).
tabs(0) --> [].

writecode(Language, Spec, Name, Sign) :-
    gencode(Language, Name, Spec, Sign, Code),
    suffix(Language, Suffix),
    concat_atom([Name, Suffix],Filename),
    tell(Filename),
    flat_format(Code, FlatCode,[]),
    maplist(write, FlatCode),
    told.

diffeq(octave, ODEs) :-
    odes(Stmts),
    findall(['xdot(',I,')= ', octave(S)], nth0(I,Stmts,_:S), ODEs).

diffeq(odespy, ['\treturn [\n',ODEs,']\n']) :-
    odes(Stmts),
    findall([S,',\n'], member(_:S,Stmts), ODEs).

:- ( current_prolog_flag(argv,[A|_])
    -> consult(A)
    ; assert(odes([
	       host0:'h0*fr*vol/60 +(log(2)/lkg)*x(1)-fr*x(1)/60-(lad/vol)*x(4)*x(1)',
	       hosta:'(lad/vol)*x(4)*x(1)+(log(2)/kgp)*x(2)-fr*x(2)/60 - x(2)/lec',
	       hostp:'x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60',
	       phage:'lpp*x(3)/60                - fr*x(4)/60',
	       phagee:'lpfraction*x(4)/60                - fr*x(5)/60']))
   ),
   genmodels(octave),
   genmodels(odespy),
   halt.
    

