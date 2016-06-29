% This is the assignment 3 in Prolog - Initially I thought of doing the second option
% but then prolog looked easier. I might do second option as well (If I get sometime later today)
% Details 
% Student : Jignesh (Jiggy) Kakkad
% Number : z3327888
% Group : 27 (I hope I am right!)

% Following text is copied from the assignment PDF
%	[1 mark] Write a Prolog procedure trigger(Events, Goals) which takes a listof                
%	events, each of the form junk(X,Y,S), and computes the corresponding list of 
%	goals for the agent, each of the form goal(X,Y,S). This is a very simple  	  
%	procedure!


% Description of the following functions:
% As it says it is easier one (Thanks God!) the first one is the base case
% Meaning if the input lists are empty then don’t do anything and simply returns
% Second is the main the processing one 
% It parses list into [H|and rest of the list] and calls the same function again 
% Also, copies into the Goal list. 

trigger([], []).

trigger([junk(X, Y, S)|Junk_Tail], [goal(X, Y, S)|A_Goal_List]) :-
    trigger(Junk_Tail, A_Goal_List).

% Following text is copied from the assignment PDF
% [4 marks] Write a Prolog procedure
%incorporate_goals(Goals, Beliefs, Intentions, Intentions1)
% which has four arguments: a list of goals each of the form goal(X,Y,S), a list of beliefs (containing one term of the form at(X,Y)), the current list of intentions each of the form [goal(X,Y,S), Plan], % and a list to be computed which contains the new goals inserted into the current list of intentions in decreasing order of value, using the distance from the agent to break ties. More precisely, a new  % goal should be placed immediately before the first goal in the list that has a lower value or which has an equal value and is further away from the agent's current position, without reordering the 
% current list of goals. Note that because of repeated perception of the same event, only new goals should be inserted into the list of intentions. The plan associated with each new goal should be the 
% empty plan (represented as the empty list).
%



% This is a helper function to check the member is a part of the list or not. 
% This context, this is to check if the Goal has been already added or already part of the intention list

% So the first helper function check if the the input first member is the head (this can be considered as the base)
% case to break the recursion 

check_if_member(Member_To_Be_Checked, [Head|_]) :- 
	member(Member_To_Be_Checked, Head).

% This is the second and the main function and it traverse through the input list and keep calling itself 
% until the above function return thru 

check_if_member(Member_To_Be_Checked, [Head|Tail]) :- 
	not(member(Member_To_Be_Checked, Head),
		check_if_member(Member_To_Be_Checked, [Tail]).

% The following functions are part of the requirement, as stated in the question that we need to add a goal in the current list of intentions 
% In decreasing order. 
% insert_a_goal is the helper function to achieve this task.
% The First version is the base case, If the input list is empty do nothing 
% Second version of the insert_a_goal,is basically check the Goal and its plan using the S of Goal(X,Y,S) 
% check_if_goal_greater_than_plan is the helper function to check the distances from the X,Y 
% check_if_goal_greater_than_plan is to achieve the second part of the question (to insert the goal in decreasing order)


insert_a_goal(X, Intentions, _, [[X, []]|Intentions]).

insert_a_goal(Goal, [HeadOfIntetion|TailOfIntentions], Belief, [Intent|Intentions1]):-
    not(check_if_goal_greater_than_plan(Goal, HeadOfIntetion, Belief)), !, 
    insert_goal(Goal, Intentions, Belief, Intentions1).

check_if_goal_greater_than_plan(goal(_, _, SvalueOne), [goal(_, _, SValueTwo)|_], _) :-
    SvalueOne > SValueTwo.

check_if_goal_greater_than_plan(goal(X1, Y1, SForGoalOne), [goal(X2, Y2, SForGoalTwo)|_], [at(X, Y)|_]) :-
    SForGoalOne == SForGoalTwo,
    distance((X, Y), (X1, Y1), DistanceWithGoalOne),
    distance((X, Y), (X2, Y2), DistanceWithGoalTwo),
    DistanceWithGoalOne < DistanceWithGoalTwo.

% From here on, the required procedures, most of the tasks and checks are done by the helper function 
% First version is the base case. Prolog style (huh!) need to break the recursion by checking the input list empty or not

incorporate_goals([], _, Intentions, Intentions1).

% Following two versions are to insert the goals 

incorporate_goals([Goal|Tail], Belief, Intentions, Intentions1) :-
    check_if_member(Goal, Intentions),
    incorporate_goals(Tail, Belief, Intentions, Intentions1).

incorporate_goals([Goal|Tail], Belief, Intentions, Intentions1) :-
    not(check_if_member(Goal, Intentions)),
    insert_a_goal(Goal, Intentions, Belief, IntentionListWithNewGoalAdedFromThisMethod),
    incorporate_goals(Tail, Belief, IntentionListWithNewGoalAdedFromThisMethod, Intentions1).


% Following text is copied from the assignment PDF
% [3 marks] Write a Prolog procedure
% select_action(Beliefs, Intentions, Intentions1, Action)
% which takes the agent's beliefs (a singleton list containing a term for the agent's location) and the list of intentions, and computes an action to be taken by the agent and the updated list of 
% intentions. The intention selected by the agent is the first on the list of intentions (if any). If the first action in this plan is applicable, the agent selects this action and updates the plan to 
% remove the selected action. If there is no associated plan (i.e. the plan is the empty list) or the first action in the plan for the first intention is not applicable in the current state, the agent 
% constructs a new plan to go from its current position to the goal state and pick up the junk there (this plan will be a list of move actions followed by an pick up action), selects the first action in 
% this new plan, and updates the list of intentions to incorporate the new plan (minus the selected first action). Due to the fact that there are no obstacles in the world, the exact path the agent takes % towards the goal does not matter, so choose any convenient way of implementing this procedure.

% Following list of procedure are the helper functions 

% This is reverse the list, I think it is obvious enough. 
% As typical of Prolog need to have different versions to break the recursion. 

reverse(List, ReversedList) :-
	reverse(List, [], ReversedList).

reverse([], ReversedList, ReversedList).

reverse([Head|Tail], PartiallyReversedList, ReversedList) :-
	reverse(Tail, [Head|PartiallyReversedList], ReversedList).


% this is to check if the move is valid or not. 
% This is the helper function used in building the new plan.

check_if_valid_move(X, Y, Move) :-
    Dx is X + 1, Move = move(Dx, Y);
    Dx is X - 1, Move = move(Dx, Y);
    Dy is Y + 1, Move = move(X, Dy);
    Dy is Y - 1, Move = move(X, Dy).

% A helper function to check if the move is in the right direction. 

heuristic_to_move_right_direction(move(X, Y), goal(goalX, goalY, _), at(currentX, currentX)) :-
    distance((X, Y), (goalX, goalY), Dm),
    distance((currentX, currentX), (goalX, goalY), Dr),
    Dm < Dr.

% This is the helper function which was mentioned in the requirement. So if the agent find an action which is not applicable
% then create a new plan. 
% Again there are different versions of these to check and break the recursion 

build_new_plan(Goal, Beliefs, Plan) :-
    build_new_plan(Goal, Beliefs, [], Plan).

build_new_plan(goal(X, Y, _), [at(X, Y)], PartialPlan, Plan) :-
    reverse([pickup(X, Y)|PartialPlan], Plan).

build_new_plan(Goal, [at(X, Y)], PartialPlan, Plan) :-
    check_if_valid_move(X, Y, move(Xnew, Ynew)),
    heuristic_to_move_right_direction(move(Xnew, Ynew), Goal, at(X, Y)),
    build_new_plan(Goal, [at(Xnew, Ynew)], [move(Xnew, Ynew)|PartialPlan], Plan).

% This is get the Goal and Plan from the list of intentions. 

get_goal_and_plan([Goal|Plan], Goal, Plan). 

new_plan_without_selected_action([Action|NewPlan1], NewPlan1, Action).


% the main function of this function 
% the first version of the function is to move agent at the starting location (0,0)
% If the input list of intentions is empty the move agent into Y direction Y.!!! just BECAUSE!!!!!!!

select_action([at(0, 0)], [], [], move(1, 0)). 

select_action([at(X, Y)], [], [], move(Xnew, Ynew)) :- 
    X < 0, Xnew is X + 1, Ynew = Y;
    X > 0, Xnew is X - 1, Ynew = Y;
    Y < 0, Ynew is Y + 1, Xnew = X;
    Y > 0, Ynew is Y - 1, Xnew = X.


% this version of function is to get the first action and if it is applicable then use it. 
select_action(Beliefs, [Intentions|TailOfIntentions], [[Goal, TailofActions]| TailOfIntentions], Action) :-
	get_goal_and_plan(Intentions, Goal, [Action| TailofActions]),
	applicable(Beliefs, Action).

% in this case the selection is not applicable then in that case, do something I mean create a new Plan .. ;)

select_action(Beliefs, [Intentions|TailOfIntentions], [[Goal, NewPlan1]|TailOfIntentions], Action) :-
    get_goal_and_plan(Intentions, Goal, [NotApplicableAction|_]),
    not(applicable(Beliefs, NotApplicableAction)),
    build_new_plan(Goal, Beliefs, NewPlan),
    new_plan_without_selected_action(NewPlan, NewPlan1, Action).

select_action(Beliefs, [[Goal, []]|TailOfIntentions], [[Goal, NewPlan1]|TailOfIntentions], Action) :-
    new_plan(Goal, Beliefs, NewPlan),
    get_goal_and_plan([Goal, NewPlan], Goal, [Action|RestofActions]),
    new_plan_without_selected_action(NewPlan, NewPlan1, Action).



% Following text is copied from the assignment PDF
% [1 mark] Write two Prolog procedures
% update_beliefs(Observation, Beliefs, Beliefs1) and
% update_intentions(Observation, Intentions, Intentions1)
% to compute the lists of beliefs and intentions resulting from the agent's observations. These are very simple procedures (one line for each possible observation type)!


% Well, Again two version! ;) 
% First version is to get the Observation at (X,Y) and update the Beliefs1
% second version is to ignore the cleaned observation. ;) 

update_beliefs(Observation, Beliefs, Beliefs1) :-
	Observation = at(X,Y),
	Beliefs1 = [Observation].

update_beliefs(_, Beliefs, Beliefs).



update_intentions(_, Intentions, Intentions).

update_intentions(cleaned(X, Y), [[goal(X, Y, _)|_]|TailOfIntentions],TailOfIntentions).


