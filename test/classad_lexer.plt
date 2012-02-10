:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).

:- use_module('../src/classad_lexer.pl').

:- begin_tests(classad_lexer_ut).

test('empty string', [nondet]) :-
    lex("", T),
    assertion(T == []).

test('whitespace only', [nondet]) :-
    lex(" \t ", T),
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

test('exp notation 3', [nondet]) :-
    lex("1.e+1", T),
    assertion(T == [10.0]).

test('variable', [nondet]) :-
    lex("a", T),
    assertion(T == ['a']).

test('variable 2', [nondet]) :-
    lex("a2", T),
    assertion(T == ['a2']).

test('variable 3', [nondet]) :-
    lex("a2 b4", T),
    assertion(T == ['a2', 'b4']).

test('variables with whitespace', [nondet]) :-
    lex(" a2 b4 ", T),
    assertion(T == ['a2', 'b4']).

test('symbol =?=', [nondet]) :-
    lex("=?=", T),
    assertion(T == ['=?=']).

test('symbol ==', [nondet]) :-
    lex("==", T),
    assertion(T == ['==']).

test('all symbols', [nondet]) :-
    lex("=?==!=||&&<=>===!=()[]{},<>+-*/!:=?;.", T),
    assertion(T == ['=?=','=!=','||','&&','<=','>=','==','!=','(',')','[',']','{','}',',','<','>','+','-','*','/','!',':','=','?',';','.']).

test('expression 1', [nondet]) :-
    lex("4*a+b-c/4e1*e", T),
    assertion(T == [4, '*', 'a', '+', 'b', '-', 'c', '/', 40.0, '*', 'e']).

test('expression 2', [nondet]) :-
    lex("ifthenelse(name =!= \"fred\", name, \"wilma\")", T),
    assertion(T == ['ifthenelse', '(', 'name', '=!=', str('fred'), ',', 'name', ',', str('wilma'), ')']).

test('identifiers', [nondet]) :-
    lex("Scope._CamelCase_Ident", T),
    assertion(T == ['scope', '.', '_camelcase_ident']).

:- end_tests(classad_lexer_ut).
