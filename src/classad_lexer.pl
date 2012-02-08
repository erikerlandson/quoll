:- module(classad_lexer,
          [lex/2          % lex(+String, -TokenList)
          ]).

:- use_module(library(lists)).

% invoke the grammar rule predicates on a string to get a token list
lex(S, TL) :- tokseq(TL, S, []).

% The top level of the lexing grammar: parse a
% sequence of tokens out of a prolog string.
% a whitespace char is just consumed, and adds nothing
% to the token list: 
tokseq(L) --> wschar, tokseq(R), {L=R}.
% if you consume a token add it to the list:
tokseq(L) --> tok(T), tokseq(R), {L=[T|R]}.
% basis case: nothing but empty string is left:
tokseq(L) --> "", {L=[]}.

% consume a whitespace character:
wschar --> [C], {char_type(C,T), member(T,[white,end_of_line])}.

% strings are tokens
% put this expansion rule first, since we want anything starting with
% quotes to tokenize as a string until next appearance of quotes
tok(T) --> str(T).

% numbers are tokens
% put this token expansion rule second because some components of numbers
% can be otherwise interpreted as other kinds of tokens, and we want to avoid
% that.
tok(T) --> num(T).

% variable names are tokens
tok(T) --> var(T).

% various approved symbols/operators are tokens
% put this expansion last, to allow longer kinds of token, such as numbers,
% to take longest-token precedence as first priority.
tok(T) --> sym(T).


% expansion of string tokens
str(S) --> "\"", strseq(SS), "\"", {atom_codes(A, SS), S=str(A)}.

strseq(SS) --> regchar(C), strseq(R), {SS=[C|R]}.
strseq(SS) --> escchar(C), strseq(R), {SS=[C|R]}.
strseq(SS) --> "", {SS=[]}.

escchar(C) --> "\\", [CC], {C = CC}.
regchar(C) --> [CC], { char_type(CC, ascii), CC \= "\\", C=CC }.


% expansion of variable name tokens
var(V) --> vhead(C), vrest(R), {atom_codes(V, [C|R])}.

vhead(C) --> [CC], { char_type(CC, alpha), C=CC }.
vrest(L) --> [CC], {char_type(CC, alnum)}, vrest(R), {L=[CC|R]}.
vrest(L) --> "", {L=[]}.


% expansion of number tokens
% I am just tokenizing anything that adheres to syntax of a standard floating point
% number, so integers are included.  Only decimal radix.
num(N) --> dhead(D), drest(R), dpseq(DS), expseq(ES), { flatten([D,R,DS,ES],L), number_codes(N, L) }.

% stick an extra zero at the end because "1." causes yap conversion to blow up
dpseq(S) --> ".", drest(R), {flatten([".",R,"0"],S)}.
dpseq(S) --> "", {S=[]}.

expseq(S) --> expchar(E), expsign(ES), dhead(D), drest(R), {flatten([E,ES,D,R], S)}.
expseq(S) --> "", {S=[]}.

expchar(C) --> [CC], {member(CC, "eE"), C=CC}.

expsign(C) --> [CC], {member(CC, "+-"), C=CC}.
expsign(C) --> "", {C=[]}.

dhead(D) --> [D], {char_type(D, digit)}.
drest(L) --> dhead(D), drest(R), {L=[D|R]}.
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
