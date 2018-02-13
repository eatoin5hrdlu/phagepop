% This will generate octave and python (odespy) versions 
% usage:
% swipl -s smgen -g test


% The model is a set of differential equations representing the populations
% host0 is uninfected host
% hosta is host with adsorbed phage
% hostp is producing phage
% phage  is total phage population
% phagee is the emerging population of a selected mutation

model([
 host0('h0*fr*vol/60 +(log(2)/lkg)*x(1)-fr*x(1)/60-(lad/vol)*x(4)*x(1)'),
 hosta('(lad/vol)*x(4)*x(1)+(log(2)/kgp)*x(2)-fr*x(2)/60 - x(2)/lec'),
 hostp('x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60'),
 phage('lpp*x(3)/60                - fr*x(4)/60'),
 phage('lpfraction*x(4)/60                - fr*x(5)/60')]).

% Compute +- parameter values for range of solutions

deltaValue(octave, Sign, VN:PC, ['l',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).
deltaValue(odespy, Sign, VN:PC, ['\tl',VN,' = delta(',VN,', ',Sign,PC,'*pm);']).

% Code Wrapper Fragments:  Pre [Assignments] Mid [Equations] Post
% template(Language, Pre, Mid, Post)

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

diffeq(octave,
 ['xdot(1)= h0*fr*vol/60 +(log(2)/lkg)*x(1) - fr*x(1)/60 - (lad/vol)*x(4)*x(1);\n',
  'xdot(2) = (lad/vol)*x(4)*x(1) + (log(2)/kgp)*x(2) - fr*x(2)/60  -  x(2)/lec;\n',
  'xdot(3) =  x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60;\n',
  'xdot(4) =  lpp*x(3)/60                - fr*x(4)/60;\n']).

diffeq(octave,
 ['xdot(1)= ', octave(A), 
  'xdot(2) =', octave(B),
  'xdot(3) =', octave(C),
  'xdot(4) =', octave(D)]) :-
    model(Stmts),
			      

diffeq(odespy, ['\treturn [\n',
 odespy('h0*fr*vol/60 +(log(2)/lkg)*x(1)-fr*x(1)/60-(lad/vol)*x(4)*x(1),'),
 odespy('(lad/vol)*x(4)*x(1)+(log(2)/kgp)*x(2)-fr*x(2)/60 - x(2)/lec,'),
 odespy('x(2)/lec   + (log(2)/kgpp)*x(3)  - fr*x(3)/60,'),
 odespy('lpp*x(3)/60                - fr*x(4)/60'), ']\n' ]).

  model(Stmts),
  findall(odespy(S), member(S,Stmts), Ss),


% filename(Language, Name, FileName).	
filename(octave, Name, File) :- concat_atom([Name,'.m'],File).
filename(odespy, Name, File) :- concat_atom([Name,'.py'],File).

test :- genmodels(octave), genmodels(odespy).

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
    filename(Language, Name, Filename),
    tell(Filename),
    flat_format(Code, FlatCode,[]),
    maplist(write, FlatCode),
    told.

