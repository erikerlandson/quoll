:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- use_module(library(assoc)).
:- source.

adjacent(Q, R, G) :- neighbors(Q, G, N), member(R, N).

undirected_graph(G, U) :- edges(G, E), ugr_edges(E, [], F), add_edges(G, F, U).

ugr_edges([], E, E).
ugr_edges([P-Q|R], S, F) :- ugr_edges(R, [Q-P|S], F).

embed(G, T, E) :- vertices(G, Gn), vertices(T, Tn), list_to_assoc([], M), embed_work(G, T, Gn, Tn, M, E).

embed_work(G, T, [], Tn, M, M).
embed_work(G, T, [Ng|Gn], Tn, M, E) :- member(Nt, Tn), forall(gen_assoc(M, Nmg, Nmt), (\+adjacent(Ng, Nmg, G) ; adjacent(Nt, Nmt, T))),
    delete(Tn, Nt, Tnn), put_assoc(Ng, M, Nt, Mn), embed_work(G, T, Gn, Tnn, Mn, E).

:- vertices_edges_to_ugraph([], [1-2,2-3,3-1,1-4], G), assert(g(1,G)).
:- vertices_edges_to_ugraph([], [5-6,6-7,7-5,5-8], G), assert(g(2,G)).
