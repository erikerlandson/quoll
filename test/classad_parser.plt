:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).

:- use_module('../src/classad_lexer.pl').
:- use_module('../src/classad_parser.pl').

:- begin_tests(classad_parser_ut).

test('ident expr', [nondet]) :-
    lex("a", T),
    parse(T, E),
    assertion(E == 'a').

test('str expr', [nondet]) :-
    lex("\"a\"", T),
    parse(T, E),
    assertion(E == str('a')).

test('num expr', [nondet]) :-
    lex("42", T),
    parse(T, E),
    assertion(E == 42).

:- end_tests(classad_parser_ut).
