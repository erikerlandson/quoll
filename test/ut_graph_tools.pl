:- use_module(library(debug)).
:- use_module(library(plunit)).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).

:- use_module('../src/graph_tools.pl').

:- begin_tests(graph_tools_test).

test(t1) :- vertices_edges_to_ugraph([1], [], G),
            vertices_edges_to_ugraph([2], [], T),
            embed(G, T, E),
            assoc_to_list(E, L),
            assertion(L == [1-2]).

:- end_tests(graph_tools_test).
