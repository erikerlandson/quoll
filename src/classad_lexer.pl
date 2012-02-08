:- module(classad_lexer,
          [lex/2          % lex(+String, -TokenList)
          ]).

:- use_module(library(lists)).

lex(S, TL) :- ilex(TL, S, []).

ilex(L) --> "", {L=[]}.
ilex(L) --> wschar, ilex(R), {L=R}.
ilex(L) --> tok(T), ilex(R), {L=[T|R]}.

wschar --> [C], {char_type(C,T), member(T,[white,end_of_line])}.

% numbers are tokens
tok(T) --> num(T).

% strings are tokens
tok(T) --> str(T).

% variable are tokens
tok(T) --> var(T).

% symbols are tokens
tok(T) --> sym(T).

str(S) --> "\"", strseq(SS), "\"", {atom_codes(A, SS), S=str(A)}.

strseq(SS) --> "", {SS=[]}.
strseq(SS) --> regchar(C), strseq(R), {SS=[C|R]}.
strseq(SS) --> escchar(C), strseq(R), {SS=[C|R]}.

regchar(C) --> [CC], { char_type(CC, ascii), CC \= "\\", C=CC }.
escchar(C) --> "\\", [CC], {C = CC}.

var(V) --> vhead(C), vrest(R), {atom_codes(V, [C|R])}.

vhead(C) --> [CC], { char_type(CC, alpha), C=CC }.
vrest(L) --> "", {L=[]}.
vrest(L) --> [CC], {char_type(CC, alnum)}, vrest(R), {L=[CC|R]}.

num(N) --> dhead(D), drest(R), dpseq(DS), expseq(ES), { flatten([D,R,DS,ES],L), number_codes(N, L) }.

% stick an extra zero at the end because "1." causes yap conversion to blow up
dpseq(S) --> ".", drest(R), {flatten([".",R,"0"],S)}.
dpseq(S) --> "", {S=[]}.

expseq(S) --> expchar(E), expsign(ES), dhead(D), drest(R), {flatten([E,ES,D,R], S)}.
expseq(S) --> "", {S=[]}.

expchar(C) --> [CC], {member(CC, "eE"), C=CC}.

expsign(C) --> [CC], {member(CC, "+-"), C=CC}.
expsign(C) --> "", {C=[]}.

dhead(D) --> [CC], {char_type(CC, digit), D=CC}.
drest(L) --> [CC], {char_type(CC, digit)}, drest(R), {L=[CC|R]}.
drest(L) --> "", {L=[]}.

% symbol tokens
% to get longest-lex behavior as the first choice from
% prolog proof, define these starting with longest tokens first.
sym(T) --> "=?=", {T='=?='}.
sym(T) --> "=!=", {T='=!='}.
sym(T) --> "<=", {T='<='}.
sym(T) --> ">=", {T='>='}.
sym(T) --> "==", {T='=='}.
sym(T) --> "!=", {T='!='}.
sym(T) --> "(", {T='('}.
sym(T) --> ")", {T=')'}.
sym(T) --> "[", {T='['}.
sym(T) --> "]", {T=']'}.
sym(T) --> ",", {T=','}.
sym(T) --> "<", {T='<'}.
sym(T) --> ">", {T='>'}.
sym(T) --> "=", {T='='}.
sym(T) --> "+", {T='+'}.
sym(T) --> "-", {T='-'}.
sym(T) --> "*", {T='*'}.
sym(T) --> "/", {T='/'}.
sym(T) --> "!", {T='!'}.
sym(T) --> ":", {T=':'}.
