:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).

:- use_module('../src/classad_parser.pl').

:- begin_tests(classad_parser_ut).

test('ident expr', [nondet]) :-
    parse("a", E),
    assertion(E == 'a').

test('str expr', [nondet]) :-
    parse("\"a\"", E),
    assertion(E == str('a')).

test('num expr', [nondet]) :-
    parse("42", E),
    assertion(E == 42).

test('func 0', [nondet]) :-
    parse("f()", E),
    assertion(E == f([])).

test('func 1', [nondet]) :-
    parse("f(a)", E),
    assertion(E == f(['a'])).

test('func 2', [nondet]) :-
    parse("f(a, 1)", E),
    assertion(E == f(['a', 1])).

test('paren', [nondet]) :-
    parse("(a)", E),
    assertion(E == 'a').

test('op !', [nondet]) :-
    parse("!true", E),
    assertion(E == '!'('true')).

test('op -', [nondet]) :-
    parse("-1.0", E),
    assertion(E == '-'(1.0)).

test('op +', [nondet]) :-
    parse("+1.0", E),
    assertion(E == '+'(1.0)).

test('composed unary 1', [nondet]) :-
    parse("--1.0", E),
    assertion(E == '-'('-'(1.0))).

test('composed unary 2', [nondet]) :-
    parse("!+--1.0", E),
    assertion(E == '!'('+'('-'('-'(1.0))))).

test('arg nesting', [nondet]) :-
    parse("f(g(-1), -(-2))", E),
    assertion(E == f([g(['-'(1)]), '-'('-'(2))])).

test('* seq 1', [nondet]) :-
    parse("2 * a", E),
    assertion(E == '*'(2,a)).

test('/ seq 1', [nondet]) :-
    parse("2 / a", E),
    assertion(E == '/'(2,a)).

test('*/ seq 1', [nondet]) :-
    parse("2 * b / a", E),
    assertion(E == '/'('*'(2,b), a)).

test('*/ seq 2', [nondet]) :-
    parse("-2 * b / +a", E),
    assertion(E == '/'('*'('-'(2),b), '+'(a))).

test('*/ seq 3', [nondet]) :-
    parse("-2 * f(b) / +a", E),
    assertion(E == '/'('*'('-'(2),f([b])), '+'(a))).

test('+ seq 1', [nondet]) :-
    parse("2+a", E),
    assertion(E == '+'(2,a)).

test('- seq 1', [nondet]) :-
    parse("2-a", E),
    assertion(E == '-'(2,a)).

test('+- seq 1', [nondet]) :-
    parse("2-a+b", E),
    assertion(E == '+'('-'(2,a), b)).

test('+- seq 2', [nondet]) :-
    parse("2*a-a/3+5*b", E),
    assertion(E == '+'('-'('*'(2,a),'/'(a,3)), '*'(5,b))).

test('+- seq 3', [nondet]) :-
    parse("2*a-a/-3+-5*b", E),
    assertion(E == '+'('-'('*'(2,a),'/'(a,'-'(3))), '*'('-'(5),b))).

test('comp ==', [nondet]) :-
    parse("a   ==b", E),
    assertion(E == '=='(a,b)).

test('comp =?=', [nondet]) :-
    parse("name =?= \"fred\"", E),
    assertion(E == '=?='(name, str(fred))).

test('comp <', [nondet]) :-
    parse("-2*a   <   b + -4 - c", E),
    assertion(E == '<'('*'('-'(2),a), '-'('+'(b, '-'(4)), c))).

test('and 1', [nondet]) :-
    parse("true && false", E),
    assertion(E == '&&'(true, false)).

test('and 2', [nondet]) :-
    parse("true && false && b", E),
    assertion(E == '&&'('&&'(true, false), b)).

test('or 1', [nondet]) :-
    parse("true || false", E),
    assertion(E == '||'(true, false)).

test('or 2', [nondet]) :-
    parse("true || false || b", E),
    assertion(E == '||'('||'(true, false), b)).

test('or and 1', [nondet]) :-
    parse("2 < 3  &&  3 > 2   ||   1+2 < 1+3  &&  2+3 < 2*3", E),
    assertion(E == '||'('&&'('<'(2,3), '>'(3, 2)), '&&'('<'('+'(1,2), '+'(1,3)),'<'('+'(2,3), '*'(2,3))))).

test('assign 1', [nondet]) :-
    parse_assign("x = 2*y + g(x, y, z)", E),
    assertion(E == '='(x, '+'('*'(2,y), g([x,y,z])))).
:- end_tests(classad_parser_ut).
