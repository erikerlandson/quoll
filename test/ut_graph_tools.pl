:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).

:- use_module('../src/graph_tools.pl').

:- begin_tests(graph_tools_test).

test('embedding an empty graph to empty graph', [nondet]) :- 
    vertices_edges_to_ugraph([], [], G),
    vertices_edges_to_ugraph([], [], T),
    embed(G, T, E),
    assoc_to_list(E, L),
    assertion(L == []).

test('embedding an empty graph to nonempty graph', [nondet]) :- 
    vertices_edges_to_ugraph([], [], G),
    vertices_edges_to_ugraph([a], [], T),
    embed(G, T, E),
    assoc_to_list(E, L),
    assertion(L == []).

test('embedding nonempty -> empty should fail', [fail]) :- 
    vertices_edges_to_ugraph([1], [], G),
    vertices_edges_to_ugraph([], [], T),
    embed(G, T, _).

test('embedding single node -> single node', [nondet]) :- 
    vertices_edges_to_ugraph([1], [], G),
    vertices_edges_to_ugraph([a], [], T),
    embed(G, T, E),
    assoc_to_list(E, L),
    assertion(L == [1-a]).

test('embedding a one-edge -> triangle', [nondet]) :- 
    vertices_edges_to_ugraph([], [1-2], G),
    vertices_edges_to_ugraph([], [a-b,b-c,c-a], T),
    embed(G, T, E),
    assoc_to_list(E, L),
    assertion(L == [1-a,2-b]).

:- end_tests(graph_tools_test).
