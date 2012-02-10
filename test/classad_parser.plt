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

test('func 0', [nondet]) :-
    lex("f()", T),
    parse(T, E),
    assertion(E == f([])).

test('func 1', [nondet]) :-
    lex("f(a)", T),
    parse(T, E),
    assertion(E == f(['a'])).

test('func 2', [nondet]) :-
    lex("f(a, 1)", T),
    parse(T, E),
    assertion(E == f(['a', 1])).

test('paren', [nondet]) :-
    lex("(a)", T),
    parse(T, E),
    assertion(E == 'a').

test('op !', [nondet]) :-
    lex("!true", T),
    parse(T, E),
    assertion(E == '!'('true')).

test('op -', [nondet]) :-
    lex("-1.0", T),
    parse(T, E),
    assertion(E == '-'(1.0)).

test('op +', [nondet]) :-
    lex("+1.0", T),
    parse(T, E),
    assertion(E == '+'(1.0)).

test('composed unary 1', [nondet]) :-
    lex("--1.0", T),
    parse(T, E),
    assertion(E == '-'('-'(1.0))).

test('composed unary 2', [nondet]) :-
    lex("!+--1.0", T),
    parse(T, E),
    assertion(E == '!'('+'('-'('-'(1.0))))).

test('arg nesting', [nondet]) :-
    lex("f(g(-1), -(-2))", T),
    parse(T, E),
    assertion(E == f([g(['-'(1)]), '-'('-'(2))])).

test('* seq 1', [nondet]) :-
    lex("2 * a", T),
    parse(T, E),
    assertion(E == '*'(2,a)).

test('/ seq 1', [nondet]) :-
    lex("2 / a", T),
    parse(T, E),
    assertion(E == '/'(2,a)).

test('*/ seq 1', [nondet]) :-
    lex("2 * b / a", T),
    parse(T, E),
    assertion(E == '/'('*'(2,b), a)).

test('*/ seq 2', [nondet]) :-
    lex("-2 * b / +a", T),
    parse(T, E),
    assertion(E == '/'('*'('-'(2),b), '+'(a))).

test('*/ seq 3', [nondet]) :-
    lex("-2 * f(b) / +a", T),
    parse(T, E),
    assertion(E == '/'('*'('-'(2),f([b])), '+'(a))).

test('+ seq 1', [nondet]) :-
    lex("2+a", T),
    parse(T, E),
    assertion(E == '+'(2,a)).

test('- seq 1', [nondet]) :-
    lex("2-a", T),
    parse(T, E),
    assertion(E == '-'(2,a)).

test('+- seq 1', [nondet]) :-
    lex("2-a+b", T),
    parse(T, E),
    assertion(E == '+'('-'(2,a), b)).

test('+- seq 2', [nondet]) :-
    lex("2*a-a/3+5*b", T),
    parse(T, E),
    assertion(E == '+'('-'('*'(2,a),'/'(a,3)), '*'(5,b))).

test('+- seq 3', [nondet]) :-
    lex("2*a-a/-3+-5*b", T),
    parse(T, E),
    assertion(E == '+'('-'('*'(2,a),'/'(a,'-'(3))), '*'('-'(5),b))).

test('comp ==', [nondet]) :-
    lex("a   ==b", T),
    parse(T, E),
    assertion(E == '=='(a,b)).

test('comp =?=', [nondet]) :-
    lex("name =?= \"fred\"", T),
    parse(T, E),
    assertion(E == '=?='(name, str(fred))).

test('comp <', [nondet]) :-
    lex("-2*a   <   b + -4 - c", T),
    parse(T, E),
    assertion(E == '<'('*'('-'(2),a), '-'('+'(b, '-'(4)), c))).

:- end_tests(classad_parser_ut).
