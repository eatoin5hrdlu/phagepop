:- use_module(library(dcg/basics)).

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
# name: kg
# type: global scalar
60


read_parameters(Ps) :-
    read_file_to_codes('/home/peter/src/EvoStat/web/ES_params.txt',Codes,[]),
    parse_params(Ps,Codes,[]).

white_space --> [C], {C < 33,!}, white_space.
white_space --> [].
		
parse_params(Ps) --> "# name: ", [C], !, parse_name(Cs),
		     { atom_codes(Name,[C|Cs]) },
		     white_space,
		     parse_val(Name, Ps).

parse_params(Ps) --> [_], !, parse_params(Ps).
parse_params([]) --> [].

parse_name([C|Cs]) --> [C], {char_type(C,alnum),!}, parse_name(Cs).
parse_name([])     --> [].

parse_val(Name, [Name:Value|Ps]) -->
    "# type: global scalar", !,
    white_space,
    number(Value),
    parse_params(Ps).
parse_val(Name, Ps) --> [_], parse_val(Name,Ps).

phage_var_label(ad)  --> [ 'Adsorption Rate : '].
phage_var_label(ec)  --> [ 'Eclipse Interval : '].
phage_var_label(dur)  --> [ 'Simulation Length : '].
phage_var_label(fr) -->  [' Flow Rate : '].
phage_var_label(vol) --> [' Lagoon Volume : '].
phage_var_label(h0) --> [' Host Cell Concentration: '].
phage_var_label(p0) --> [' Initial Phage Population: '].
phage_var_label(pp) --> [' Phage Production: '].
phage_var_label(kg) --> !, [' E. coli Doubling Time : '].
phage_var_label(moi) --> [' Multiplicity of Infection : '].
phage_var_label(lsl) --> [' Linear=1, Semilog=0 : '].
phage_var_label(K)  --> { concat_atom([k,X],K) }, [' Growth Factor ',X,' : '].

% Variable prefix determines some parameter types:
% kXXX   is a growth constant
% tmXXX  a temperature in deg C
phage_var_units(ad) --> [ 'mL/min' ].
phage_var_units(kg) --> [ 'min' ].
phage_var_units(dur) --> [ 'hours' ].
phage_var_units(ec) --> [ 'min' ].
phage_var_units(fr) --> [ 'Volumes/hour' ].
phage_var_units(vol) --> [ 'mL' ].
phage_var_units(h0) --> [ 'Cells/mL' ].
phage_var_units(p0) --> [ 'Virons/mL' ].
phage_var_units(pp) --> [' Phage/Cell-hour '].
phage_var_units(moi) --> [' Phage/Cell '].
phage_var_units(lsl) --> [' 1 or 0 '].
phage_var_units(KX) --> { atom_prefix(KX,k),! }, ['t',sup(-1)].
phage_var_units(TM) --> { atom_prefix(TM,tm),! }, [&('#x2152'),sup(o),'C'].
phage_var_units(OD) --> { atom_prefix(OD,od),! }, [' OD',sub(600) ].

phage_space(0)  --> !, [].
phage_space(N)  --> {number(N), N>0, NN is N-1}, !, [&(nbsp)], phage_space(NN).
phage_space(tt) --> !, phage_space(4).
phage_space(_) --> [].

modelItems([]) --> [].
modelItems([Var:Val|Ps]) -->
    phage_var_label(Var),
    [ input([type(text),name(Var),value(Val),style('width:30%')]) ],
    phage_var_units(Var), [br([],[])], phage_space(4),
    modelItems(Ps).

phagepop(_Req) :-
    read_parameters(Ps),
    assert(html_syntax),
    modelItems(Ps,Fields,[]),
    retract(html_syntax),
    flatten([ Fields,
	input([type=submit,name=submit,value='Submit']),
	input([type=button,name=cancel,value='Cancel',
        onClick='window.location="/web/pathe.pl"'])],Inputs),
    defaultHead('Pathe Population Model',Head),
    reply_html_page(Head,
	    body([background('/web/images/brushed.jpg'),
		  style('background-size: 100% 130%')],
	     [' ',br([],[]),
	      center(a([href('/web/model.txt'),style('color:white')],
		    [font([size('+2')],'CLICK HERE TO SEE MODEL SOURCE')])),
	      center(font([size('+2'),style('color:#FDFDFD')],
			  [' ',br([],[])])),
	      form([action='./run_model.pl'], Inputs)])).

phagepop(Request) :-
    errorPage(Request, 'There was a problem generating the model').


