:- module(classad_parser,
          [parse/2           % parse(+TokenList, -ExprTree)
          ]).

:- use_module(library(lists)).

% invoke the grammar rule predicates on a token list to get an expr-tree
parse(TL, E) :- expr(E, TL, []).

expr(E) --> num(E).
expr(E) --> str(E).
expr(E) --> ident(E).

num(N) --> [N], { number(N) }.
str(S) --> [S], { S=str(_) }.
ident(I) --> [I], { atomic(I), \+number(I) }.
