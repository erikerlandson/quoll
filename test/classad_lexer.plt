:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).

:- use_module('../src/classad_lexer.pl').

:- begin_tests(classad_lexer_ut).

test('empty string', [nondet]) :-
    lex("", T),
    assertion(T == []).

test('whitespace only', [nondet]) :-
    lex("  ", T),
    assertion(T == []).

test('string token', [nondet]) :-
    lex("\"a string\"", T),
    assertion(T == [str('a string')]).

test('integer', [nondet]) :-
    lex("42", T),
    assertion(T == [42]).

test('floating point', [nondet]) :-
    lex("3.14", T),
    assertion(T == [3.14]).

test('floating point', [nondet]) :-
    lex("3.", T),
    assertion(T == [3.0]).

test('exp notation', [nondet]) :-
    lex("31.4e-1", T),
    assertion(T == [3.14]).

test('exp notation 2', [nondet]) :-
    lex("1e1", T),
    assertion(T == [10.0]).

test('symbol =?=', [nondet]) :-
    lex("=?=", T),
    assertion(T == ['=?=']).

test('symbol ==', [nondet]) :-
    lex("==", T),
    assertion(T == ['==']).

test('symbol =', [nondet]) :-
    lex("=", T),
    assertion(T == ['=']).

:- end_tests(classad_lexer_ut).
