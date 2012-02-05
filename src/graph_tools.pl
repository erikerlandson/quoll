:- module(graph_tools,
          [embed/3,               % +Graph, +TargetGraph, -Embedding
           adjacent/3,            % +NodeQ, +NodeR, +Graph
           undirected_graph/2     % +Graph, -UndirectedGraph
          ]).

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(ugraphs)).

% adjacent(+Q, +R, +G) succeeds if nodes +Q and +R are vertices of +G and are adjacent
adjacent(Q, R, G) :- neighbors(Q, G, N), member(R, N).

% undirected_graph(+G, -U) succeeds if +G is a graph, and -U is unified with a graph where if a-b is an edge in +G then both a-b and b-a will be edges in -U
undirected_graph(G, U) :- edges(G, E), ugr_edges(E, [], F), add_edges(G, F, U).

ugr_edges([], E, E).
ugr_edges([P-Q|R], S, F) :- ugr_edges(R, [Q-P|S], F).

% embed(+G, +T, -E) succeeds when -E will be unified with a 1-1 mapping of all vertices of graph +G into verticies of target graph +T, such that edge a-b element of +G implies edge E(a)-E(b) is an edge in graph +T, for all a-b in +G.
embed(G, T, E) :- vertices(G, Gn), vertices(T, Tn), list_to_assoc([], M), embed_work(G, T, Gn, Tn, M, E).

% Basis case: no more nodes from G to map into T, send embedding 'M' back up the stack
embed_work(_, _, [], _, M, M).

% Try out a mapping from a node in G to a node in T, and see if it preserves edges.
% If it does preserve edges, then move on to the next pair of nodes.
embed_work(G, T, [Ng|Gn], Tn, M, E) :- 
    member(Nt, Tn), 
    forall(gen_assoc(M, Nmg, Nmt), 
           ((\+adjacent(Ng, Nmg, G) ; adjacent(Nt, Nmt, T)), (\+adjacent(Nmg, Ng, G) ; adjacent(Nmt, Nt, T)))
    ),
    delete(Tn, Nt, Tnn), 
    put_assoc(Ng, M, Nt, Mn), 
    embed_work(G, T, Gn, Tnn, Mn, E).
