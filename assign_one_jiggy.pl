% Name : Jiggy Kakkad (Jignesh Kakkad) 
% Student Number: z3327888
% Assignment : Assignment 1 - Prolog Programming
% Coming from Java background, I find it bit difficult to put everything in one functor. So my aim would be write 
% helper predicates. 



% Question 1 
% The helper predicate to check if an element from the given list is an even or not

is_element_even(Element) :- 
	0 is mod(Element, 2).

% This predicate does the same thing but checks for the odd number. 

is_element_odd(Element) :- 
	Temp_Result is mod(Element, 2), 
	Temp_Result =\= 0.

% Helper predicate to get a sqr of the given number 
% Value is kind of an output variable (or the variable which will contains the result) and 
% the Element will be seq. 

get_me_the_sq(Element, Value) :-
	Value is Element * Element.

% The main predicate 

% This fact - if the input list an empty list then the result is ZERO. 
sumsq_even([], 0).

sumsq_even([HeadOfTheList|RestOfTheElements], Sum) :-
	is_element_even(HeadOfTheList),
	sumsq_even(RestOfTheElements, TempSum),
	get_me_the_sq(HeadOfTheList, SqOfHeadOfTheList),
	Sum is SqOfHeadOfTheList+TempSum.

% Right so this version of predicate to handle the odd numbers
% The way prolog works, previous predicate stops when it finds an odd number 
% So we need to have a mechanism to bypass that and keep continue with 
% other elements 

sumsq_even([HeadOfTheList|RestOfTheElements], Sum) :-
        is_element_odd(HeadOfTheList),
        sumsq_even(RestOfTheElements, Sum).


%Question 2 
% This is another helper predicate to traverse through the list and checks the 
% person like the given fruit or not. 
% the reason for writing this predicate - not sure how to get head and tail when 
% the input is just List (instead of Head | Tail)

helper_all_like(What, [Head|Tail]) :-
	likes(Head, What),
	helper_all_like(What, Tail).

helper_all_like(What, []) :-
	What =:= What.
 
all_like(What, List) :-
	helper_all_like(What, List). 



% Below predicates are for the 3rd question 
% Wow! I mean WOW! In Prolog, everything needs say True or YES and somehow we will need find a way to break 
% the loop in this case recursion
% So the first one is going to kind of break the recursion and returns the Element, ElementSQRT pair
% while the second is going through the loop until N > M is true

sqrt_table(N,M,Result):-
    N=:=M,
    TempSqrt is sqrt(N),
    Result = [[N,TempSqrt]].

sqrt_table(N, M, Result) :-
    	N > M,
        TempSqrt is sqrt(N),
        TempList = [N, TempSqrt],
    	TempN is N - 1,
    	sqrt_table(TempN,M,TempResult),
        append([TempList], TempResult, Result).

% Question 4 
% Again following my style of coding by having a seperate functor for a small tasks 
% The below functor is a helper to check whether the number and the head are sequential 
% If it is, then it is going to return true. ! 
	
is_it_the_next_number(Number, [Head|_]) :-
	Number - 1 =:= Head.

% A Fact 
chop_down([], []).

% This is the main predicate which traverse through the list and check each and every Element
% For this one it expect the is_it_the_next_number return true i.e. Head (which is the First Number) 
% and the tail (First element from this tail) are descreasing sequential. 
% The next predicate does the same thing however, it considers the false case of is_it_the_next_number.

chop_down([Head|Tail], NewList) :-
	is_it_the_next_number(Head, Tail),
	chop_down(Tail, NewList).

chop_down([Head|Tail], [Head|NewList]) :-
	not(is_it_the_next_number(Head, Tail)),
	chop_down(Tail, NewList).


% Question 5

% This is the main predicate. This was a bit difficult one to implement. however, manage to get 
% around the solution. Phew!! 
% First two are the facts given in the question.
% The last one, traverses through the left tree and right tree and evolutate the expressions. 

tree_eval(Value, tree(empty, z, empty), Value).

tree_eval(_, tree(empty, Num, empty), Num).

tree_eval(Value, tree(LeftTree, Operator, RightTree), Eval) :-
	tree_eval(Value, LeftTree, L),
	tree_eval(Value, RightTree, R),
	Expression =.. [Operator, L, R],
	Eval is Expression.


