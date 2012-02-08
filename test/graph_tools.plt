:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(maplist)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).

:- use_module('../src/graph_tools.pl').

:- begin_tests(graph_tools_ut).

test('embedding an empty graph to empty graph', [nondet]) :- 
    vertices_edges_to_ugraph([], [], G),
    vertices_edges_to_ugraph([], [], T),
    findall(E, embed(G, T, E), L),
    maplist(assoc_to_list, L, LL),
    assertion(LL == [[]]).

test('embedding an empty graph to nonempty graph', [nondet]) :- 
    vertices_edges_to_ugraph([], [], G),
    vertices_edges_to_ugraph([a], [], T),
    findall(E, embed(G, T, E), L),
    maplist(assoc_to_list, L, LL),
    assertion(LL == [[]]).

test('embedding nonempty -> empty should fail', [nondet]) :- 
    vertices_edges_to_ugraph([1], [], G),
    vertices_edges_to_ugraph([], [], T),
    findall(E, embed(G, T, E), L),
    maplist(assoc_to_list, L, LL),
    assertion(LL == []).

test('embedding single node -> single node', [nondet]) :- 
    vertices_edges_to_ugraph([1], [], G),
    vertices_edges_to_ugraph([a], [], T),
    findall(E, embed(G, T, E), L),
    maplist(assoc_to_list, L, LL),
    assertion(LL == [[1-a]]).

test('embedding a one-edge -> triangle', [nondet]) :- 
    vertices_edges_to_ugraph([], [1-2], G),
    vertices_edges_to_ugraph([], [a-b,b-c,c-a], T),
    findall(E, embed(G, T, E), L),
    maplist(assoc_to_list, L, LL),
    assertion(LL == [[1-a,2-b],[1-b,2-c],[1-c,2-a]]).

:- end_tests(graph_tools_ut).
