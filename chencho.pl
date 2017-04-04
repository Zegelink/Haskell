%Author: Chongxian Chen 
%Date: March 14, 2017

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_).

isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,M), parent(M,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(M,X), parent(M,Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y), male(X).
sister(X,Y) :- sibling(X,Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(X,M), married(Y,M); sibling(Y,M), married(X,M).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- (siblingInLaw(X, M); sibling(X,M)), female(X), parent(M,Y).
uncle(X,Y) :- (siblingInLaw(X, M); sibling(X,M)), male(X), parent(M,Y).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(M,X), sibling(N,M), parent(N,Y), N\=M.

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y) ; parent(M,Y), ancestor(X,M).

% Extra credit: Define the predicate `related/2`.
% related(X,Y) :-


%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

cmd(add, [A,B|R],S2) :- X is (A+B), S2= [X|R].
cmd(lte, [A,B|R],S2) :- X = (A =< B -> Y =t; Y=f), call(X), S2=[Y|R].
cmd(if(R,_),[t|T],S2) :- prog(R,T,S2).
cmd(if(_,W),[f|T],S2) :- prog(W,T,S2).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
prog([], S1,S2) :- S2 = S1.
prog([C|T],S1,S2):- cmd(C,S1,S3),prog(T,S3,S2).

