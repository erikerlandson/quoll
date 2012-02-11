:- module(classad_parser,
          [parse/2,            % parse(+String, -ExprTree)
           parse_tl/2          % parse_tl(+TokenList, -ExprTree)
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(classad_lexer).

% lex and parse a string to get an expression list
parse(S, E) :-
    classad_lexer:lex(S, TL),
    parse_tl(TL, E).

% invoke the grammar rule predicates on a token list to get an expr-tree
parse_tl(TL, E) :- expr(E, TL, []).

% reserved words in the classad spec
reserved_expr(A) :- member(A, ['true', 'false', 'parent', 'undefined', 'error']).
reserved_op(A) :- member(A, ['is', 'isnt']).

expr(E) --> cond(E).

cond(E) --> orseq(C), condrest(C, E).
condrest(C, E) --> ['?'], expr(RT), [':'], expr(RF), { E = '?:'(C, RT, RF) }.
condrest(C, C) --> [].

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
muldivrest(SE, E) --> [OP], { member(OP, ['*','/','%']) }, unary(SE2), { TE =.. [OP,SE,SE2] }, muldivrest(TE, E).
muldivrest(E, E) --> [].

unary(E) --> [OP], { member(OP,['!','-','+']) }, unary(SE), { E =.. [OP,SE] }.
unary(E) --> idxseq(E).

idxseq(E) --> selseq(SE), idxrest(SE, E).
idxrest(SE, E) --> ['['], expr(SE2), [']'], { TE = '[]'(SE,SE2) }, idxrest(TE, E).
idxrest(E, E) --> [].

selseq(E) --> atomic(SE), selrest(SE, E).
selrest(SE, E) --> ['.'], ident(SE2), { TE = '.'(SE,SE2) }, selrest(TE, E).
selrest(E, E) --> [].

% order matters here:  
% e.g. we definitely want func(E) before paren(E) and ident(E).
% fun fact: abstime and reltime literals are not part of the grammar.
atomic(E) --> classad(E).
atomic(E) --> list(E).
atomic(E) --> func(E).
atomic(E) --> paren(E).
atomic(E) --> reserved(E).
atomic(E) --> num(E).
atomic(E) --> str(E).
atomic(E) --> ident(E).

% a classad is a sequence of assignments: var = expr;
% I load these into an association list from the standard assoc library
classad(E) --> ['['], { list_to_assoc([], Mi) }, assignseq(Mi, Mo), [']'], { E = '[classad]'(Mo) }.
assign(Mi, Mo) --> ident(V), ['='], expr(E), [';'], { put_assoc(V, Mi, E, Mo) }.
assignseq(Mi, Mo) --> assign(Mi, Mt), assignseq(Mt, Mo).
assignseq(M, M) --> [].

% a list is a comma-separated sequence of expressions between {}.
list(E) --> ['{'], exprseq(E), ['}'].

% function calls are of the typical form: f(a1, a2, ...)
func(E) --> ident(F), ['('], exprseq(A), [')'], { E=..[F,A] }.

% a comma-separated sequence of expressions, possibly empty
exprseq(S) --> expr(E), exprrest(R), { S=[E|R] }.
exprseq([]) --> [].
exprrest(S) --> [','], expr(E), exprrest(R), { S=[E|R] }.
exprrest([]) --> [].

% parenthesized sub-expressions:
paren(E) --> ['('], expr(E), [')'].

% reserved identifiers that are valid atomic expression values
% does not include reserved words that are operators
reserved(R) --> [R], { reserved_expr(R) }.

% numbers, strings, identifiers:
num(N) --> [N], { number(N) }.
str(S) --> [S], { S=str(_) }.
ident(I) --> [I], { atomic(I), \+number(I), \+reserved_expr(I), \+reserved_op(I) }.
