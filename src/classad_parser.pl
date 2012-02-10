:- module(classad_parser,
          [parse/2,           % parse(+TokenList, -ExprTree)
           parse_assign/2     % parse_assign(+TokenList, -ExprTree)
          ]).

:- use_module(library(lists)).

% invoke the grammar rule predicates on a token list to get an expr-tree
parse(TL, E) :- expr(E, TL, []).
parse_assign(TL, E) :- assign(E, TL, []).

% a classad assignment to a variable
assign(A) :- ident(V), ['='], expr(E), { A = '='(V, E) }.

expr(E) --> orseq(E).

orseq(E) --> andseq(SE), orrest(SE, E).
orrest(SE, E) --> [OP], { member(OP, ['||']) }, andseq(SE2), { TE =.. [OP,SE,SE2] }, orrest(TE, E).
orrest(E, E) --> [].

andseq(E) --> comp(SE), andrest(SE, E).
andrest(SE, E) --> [OP], { member(OP, ['&&']) }, comp(SE2), { TE =.. [OP,SE,SE2] }, andrest(TE, E).
andrest(E, E) --> [].

comp(E) --> addsubseq(SE1), [OP], { member(OP, ['==','!=','<=','>=','<','>','=?=','=!=']) }, addsubseq(SE2), { E =.. [OP,SE1,SE2] }.
comp(E) --> addsubseq(E).

addsubseq(E) --> muldivseq(SE), addsubrest(SE, E).
addsubrest(SE, E) --> [OP], { member(OP, ['+','-']) }, muldivseq(SE2), { TE =.. [OP,SE,SE2] }, addsubrest(TE, E).
addsubrest(E, E) --> [].

muldivseq(E) --> unary(SE), muldivrest(SE, E).
muldivrest(SE, E) --> [OP], { member(OP, ['*','/']) }, unary(SE2), { TE =.. [OP,SE,SE2] }, muldivrest(TE, E).
muldivrest(E, E) --> [].

unary(E) --> [OP], { member(OP,['!','-','+']) }, unary(SE), { E =.. [OP,SE] }.
unary(E) --> atomic(E).

atomic(E) --> func(E).
atomic(E) --> paren(E).
atomic(E) --> num(E).
atomic(E) --> str(E).
atomic(E) --> ident(E).

func(E) --> ident(F), ['('], argseq(A), [')'], { E=..[F,A] }.

argseq(A) --> expr(E), argrest(R), { A=[E|R] }.
argseq(A) --> [], { A=[] }.
argrest(A) --> [','], expr(E), argrest(R), { A=[E|R] }.
argrest(A) --> [], { A=[] }.

paren(E) --> ['('], expr(E), [')'].

num(N) --> [N], { number(N) }.
str(S) --> [S], { S=str(_) }.
ident(I) --> [I], { atomic(I), \+number(I) }.
