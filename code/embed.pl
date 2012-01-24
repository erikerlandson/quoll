:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- source.

:- vertices_edges_to_ugraph([], [1-2,1-3,1-4,4-5], G).

undirected_graph(G, U) :- edges(G, E), ugr_edges(E, [], F), add_edges(G, F, U).

ugr_edges([], E, E).
ugr_edges([P-Q|R], S, F) :- ugr_edges(R, [Q-P|S], F).
