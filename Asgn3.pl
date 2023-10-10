% Be sure to replace the words "UCID" in this file with your own UCID

% Guidelines for this assignment.
% Your top-level relations for each question (the relation you are
% being asked to implement but not the sub-relations you used to make it)
% must return all possible correct answers and no incorrect answers.
% This means repeating correct answers is acceptable.
% We will also test multiple modes which will be stated for each question.
% A + indicates that this is input while a - indicates it is output.

% For some modes, we actually want the predicate to never terminate.
% For example, for member(5,Xs) we would expect the predicate to
% never terminate as there is an infinite amount of possible lists Xs
% that could satisfy this relationship.

% For other modes, we want the predicate to terminate.
% For example, for member(5,[]) we want it to return true and then halt.

% If for a given input your answer returns more than one answer and then false
% then the final false will be ignored.
% If for a given input your answer only returns a false, then it will be counted
% as rejecting the input
% For example:
% ?- list_only([],5,[])
%    true.
% and
% ?- list_only([],5,[]).
%    true.
%    false.
% would both be counted as correct but
% ?- list_only([],5,[]).
%    false.
% would not be as the only output was false.



%-----------------------------------------------



% 1.
% Base Case:
%	If the first two params are empty, then return the empty list
%	If the first param is an empty list, then return the second param (nothing to riffle with)
%	If the second param is an empty list, then return the first param
%	
%	
% Inductive step:
%	If the first and second param are lists, then recurse with the tail of the first param and
%	the param of the second list. Eventually the result will be returned in R, in which case we
%	can calculate Zs which is the result of concat R to Y head and X head.
%		
riffle([], [], []).
riffle([], Ys, Ys).
riffle(Xs, [], Xs).
riffle([X|XS], [Y|YS], Zs) :- riffle(XS, YS, R), Zs = [Y,X|R].




%  2.
%  Base Case:
%	If the first param is an empty list, followed by any value, then return the empty list
% 	(there is no value to search for in an empty list)
%	
% Inductive step:
%	If the head of list is equal to the value we are trying to search for (T), then recurse on
% 	the tail of the list with T. Eventually the recursive step will return inside R and we can 
% 	calculate Ys: the resulting list only containing T.
% 	If the head of the list is not equal to the value we are trying to search for, then recurse on
% 	the tail of the list with T. However, this time, the R returned will be the current/previous value
% 	of R alone and not concat with the head b/c the head \= the value we are searching for
list_only([], _, []).
list_only([X|XS], T, Ys) :- (X = T), list_only(XS, T, R), (Ys = [X|R]).
list_only([X|XS], T, Ys) :- (X \= T), list_only(XS, T, R), (Ys = R).




% 3.
% Base Case:
%	If the first param is equal to the empty list rhen return the empty list b/c
%	there is no negatives to take out
%	
%	
% Inductive step:
%	If the head of the list X >= 0, then recurse on the tail of the list. Eventually
%	Zs will contain the result, in which case, to calculate Ys (so the list contains only positive numbers), 
%	take the head of the list and concat with the return of the inductive step, Zs.
%	
%	If the head of the list X < 0 then continue to recurse on the list without removing anything
%	from the list (b/c its negative)
%	
neg_only([], []).
neg_only([X|Xs], Ys) :- (X >= 0) -> neg_only(Xs, Zs), (Ys = [X|Zs]).
neg_only([X|Xs], Ys) :- (X < 0) -> neg_only(Xs, Ys).




% 4.
leaf(_).
gnode([]).

% list of gnodes starting with a leaf followed by a gnode component (leaf or another gnode)
% this is only a valid gnode if gnode(T) evaluates to True (tail must not be a list of gnodes, but a gnode structure alone)
gnode([leaf(_)|T]):- gnode(T).

% list of gnodes starting with a leaf followed by a gnode component (leaf or another gnode)
% this is only a valid gnode if BOTH gnode(X) AND gnode(T) evaluate to True
gnode([gnode(X)|T]):- gnode(X), gnode(T).


% Base Case:
%   If the param is a leaf containing some value X, then return the value in that leaf
%   If the param is a gnode containing an empty list, then return the empty list
%
% Inductive Step:
%   If the param is a gnode list starting with the leaf (containing some value V) 
%   as the head, then recurse on gtree_list with the tail T of the list wrapped in a gnode
%   (b/c we must pass in not just a list, but a gnode component) and a value R (R will eventually
%   contain the return of this recusive step). Once this predicate returns R, calculate the value of
%   the list, which is the head V (value inside leaf) concat with the return of the recursive call.
%
%	If the param is a gnode list starting with a gnode (containing some value X: could be a 
%	leaf or another gnode) as the head, then recurse on:
%		- gtree_list with the gnode X and some value R1 (b/c we must check if the head is a valid gnode)
%		- gtree_list with the gnode T and some value R2 (b/c we must check if the Tail gnode list contains valid 
%		  gnode elements)
%	Once both predicates return in the form of R1 and R2, then we can calculate the list by concat R1 with R2
%   By recursing on the value inside gnode, it effectively reduces the function by one level
%
gtree_list(leaf(X), X).
gtree_list(gnode([]), []).

gtree_list(gnode([leaf(V)|T]), [V|R]) :- 
    gtree_list(gnode(T), R).

gtree_list(gnode([gnode(X)|T]), [R1|R2]) :- 
    gtree_list(gnode(X), R1), 
    gtree_list(gnode(T), R2).



% 5.
succ(succ(X)) :- succ(X).
succ(nat_zero). 
nat_zero.

% Base Case:
%	If the param is 0, then return the Nat representation of zero
%	If the param is 1, then return the Nat representaiton of one 
%	
% Inductive step:	
%	If the param is any number >= 2, then check Int >= 0 to ensure it is not negative. Now,
% 	to reduce the function, recurse on Int2 (which is Int decreased by one) and recurse on one
% 	value less than succ(succ(Nat)) which is suc(Nat). Here we wrap the result of the recursive
% 	call in succ. The representation is succ(succ(...succ(0)...)) where the number of succs in 
% 	the representation corresponds to the value of Int.
% 	If the input is negative, then Int >=0 will evaluate to false (making the whole line false), 
% 	thereby returning False.
%
int_nat(0, nat_zero) :- !.
int_nat(1, succ(nat_zero)) :- !.
int_nat(Int, succ(succ(Nat))) :- Int >= 0, Int2 is Int - 1, int_nat(Int2, succ(Nat)).





% Here is a set of facts and rules to represent binary numbers:
bin(bin(X),bin(Y,Z)):-
    bin(X),
    bin(Y,Z).
bin(bin(X),bin(Y)):-
    bin(X),
    bin(Y).
bin(zero).
bin(one).


% 6.
% I was only able to have it work up to a bitstring of size 3 i.e.
% bin_nat(bin(bin(one), bin(bin(one), bin(one)))
% 
% Base Case:
%	If the param is bin(zero), then return the Nat representation of 1
%	If the param is bin(one), then return the Nat representaiton of 0 
%	
% Inductive step:	
%	If the param is a binary number, say, the most signifficant bit (MSB) X, followed by 
%	another bit Y, then we want to recurse on each bit, retuning the result in R1 and R2
%	respectively as Nat represntation. After, a nat_helper is calledd w/ both Nat representations
%	R1 and R2 in order to compose the Nat numbers together.
%		If the both params are zero, then return nat_zero
%		If one param is in Nat representation but the other is nat_zero, then return the Nat number
%		
%		If both params are non zero Nats, then recurse on nat_helper with R1 and R2. Eventually,
%		the recursion will backtrack so that R1 and R2 are composed with each other so that
%		they represent the appropriate binary number. This will be stored in Result and wrapped in
%		two succcessor structures
%		
%	If the param is a binary number that is the MSB X, followed by a binary number with two more remaing 
%	bits then recurse seperately on X and Y with Z. The same process described above will happen. Although,
%	when we recurse on Y and Z the two Nat represenatations will eventually be combined with each other
%	in nat_helper, and finally, we will compose the result with Xs Nat representation in nat_helper too.
%	
%
bin_nat(bin(zero), nat_zero).
bin_nat(bin(one), succ(nat_zero)).

bin_nat(bin(bin(X), bin(Y)), succ(Result)) :- 
    bin_nat(bin(X), R1), 
    bin_nat(bin(Y), R2),
    nat_helper(R1, R2, Result).

bin_nat(bin(bin(X), bin(bin(Y), bin(Z))), succ(succ(succ(Result)))) :- 
    bin_nat(bin(X), R1), 
    bin_nat(bin(bin(Y), bin(Z)), R2),
	nat_helper(R1, R2, Result).
    
nat_helper(nat_zero, nat_zero, nat_zero) :- !.
nat_helper(nat_zero, succ(R2), succ(R2)) :- !.
nat_helper(succ(R1), nat_zero, succ(R1)) :- !.
nat_helper(succ(R1), succ(R2), succ(succ(Result))) :- nat_helper(R1, R2, Result).