% name surname: Yusuf Suat Polat
% id: 2021400312
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.

agents_distance(Agent1,Agent2, Distance) :-

    get_dict(x, Agent1, X1),
    get_dict(y, Agent1, Y1),
    get_dict(x, Agent2, X2),
    get_dict(y, Agent2, Y2),
    Distance is abs(X1-X2)+abs(Y1-Y2).

number_of_agents([Agents, _, _, _], NumberOfAgents) :-
    is_dict(Agents),          % Check if Agents is a dictionary
    dict_keys(Agents, Keys),  % Get all the keys from the Agents dictionary
    length(Keys, NumberOfAgents).  % Count the number of keys, which represents the number of agents
    
value_of_farm(State,Value):-
    state(Agents, Objects, _, _),
    findall(Subtype,(dict_pairs(Agents, _, Pairs), member(_-Agent, Pairs), get_dict(subtype, Agent, Subtype)),Items_Agents),
    sum_values(Items_Agents,Value_Agents),
    findall(Subtype,(dict_pairs(Objects, _, Pairs), member(_-Object, Pairs), get_dict(subtype, Object, Subtype)),Items_Objects),
    sum_values(Items_Objects,Value_Objects),
    Value is Value_Objects + Value_Agents.

sum_values(List, TotalValue) :-
    sum_values_helper(List, 0, TotalValue).

sum_values_helper([], Accumulator, Accumulator).
sum_values_helper([Item|Rest], Accumulator, TotalValue) :-
    % Retrieve the value of the current item
   % Retrieve the value of the current item, defaulting to 0 if not found
    (   value(Item, ItemValue)
    ->  true
    ;   ItemValue = 0  % Use 0 if the value predicate does not exist for the item
    ),
    % Update the accumulator
    NewAccumulator is Accumulator + ItemValue,
    % Recurse over the rest of the list
    sum_values_helper(Rest, NewAccumulator, TotalValue).
    
find_food_coordinates(State, AgentId, Coordinates):-
    state(Agents, Objects, _, _),
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype,Agent,EaterSubtype),
    findall(Edible, can_eat(EaterSubtype,Edible),Edible_Types),
    findall(Object,
            (   dict_pairs(Objects, _, Pairs),
                member(_-Object, Pairs),
                get_dict(subtype, Object, Subtype),
                member(Subtype, Edible_Types)  % Check if the subtype is in the list
            ),
            FilteredObjects),
    findall([X, Y],                      % Template for output list elements
            (member(Object, FilteredObjects),    % Iterate over each object in the list
             get_dict(x, Object, X),     % Retrieve the X coordinate
             get_dict(y, Object, Y)      % Retrieve the Y coordinate
            ),
            ObjectCoordinates),
    findall(EdibleAgent,
            (   dict_pairs(Agents, _, Pairs),
                member(_-EdibleAgent, Pairs),
                get_dict(subtype, EdibleAgent, Subtype),
                member(Subtype, Edible_Types)  % Check if the subtype is in the list
            ),
            FilteredAgents),
    findall([X, Y],                      % Template for output list elements
            (member(Object, FilteredAgents),    % Iterate over each object in the list
             get_dict(x, Object, X),     % Retrieve the X coordinate
             get_dict(y, Object, Y)      % Retrieve the Y coordinate
            ),
            AgentCoordinates),
    % writeln('Edible items:'),
    % writeln(FilteredObjects),
    % writeln('Edible item coordinates:'),
    % writeln(ObjectCoordinates),
    % writeln('---------------------------'),
    % writeln('Edible Agents:'),
    % writeln(FilteredAgents),
    % writeln('---------------------------'),
    % writeln('Edible Agent coordinates:'),
    % writeln(AgentCoordinates),
    append(ObjectCoordinates,AgentCoordinates,Coordinates).

find_food_coordinates_with_types(State, AgentId, AgentCoordinates, ObjectCoordinates, Coordinates):-
    state(Agents, Objects, _, _),
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype,Agent,EaterSubtype),
    findall(Edible, can_eat(EaterSubtype,Edible),Edible_Types),
    findall(Object,
            (   dict_pairs(Objects, _, Pairs),
                member(_-Object, Pairs),
                get_dict(subtype, Object, Subtype),
                member(Subtype, Edible_Types)  % Check if the subtype is in the list
            ),
            FilteredObjects),
    findall([X, Y],                      % Template for output list elements
            (member(Object, FilteredObjects),    % Iterate over each object in the list
             get_dict(x, Object, X),     % Retrieve the X coordinate
             get_dict(y, Object, Y)
            ),
            ObjectCoordinates),
    findall(EdibleAgent,
            (   dict_pairs(Agents, _, Pairs),
                member(_-EdibleAgent, Pairs),
                get_dict(subtype, EdibleAgent, Subtype),
                member(Subtype, Edible_Types)  % Check if the subtype is in the list
            ),
            FilteredAgents),
    findall([X, Y],                      % Template for output list elements
            (member(Object, FilteredAgents),    % Iterate over each object in the list
             get_dict(x, Object, X),     % Retrieve the X coordinate
             get_dict(y, Object, Y)
            ),
            AgentCoordinates),
    % writeln('Edible items:'),
    % writeln(FilteredObjects),
    % writeln('Edible item coordinates:'),
    % writeln(ObjectCoordinates),
    % writeln('---------------------------'),
    % writeln('Edible Agents:'),
    % writeln(FilteredAgents),
    % writeln('---------------------------'),
    % writeln('Edible Agent coordinates:'),
    % writeln(AgentCoordinates),
    append(ObjectCoordinates,AgentCoordinates,Coordinates).


find_nearest_agent(State, AgentId, Coordinates, NearestAgent):-
    state(Agents, Objects, _, _),
    get_dict(AgentId, Agents, Agent),
    get_dict(x, Agent, X1),
    get_dict(y, Agent, Y1),

    findall(EdibleAgent,
            (   dict_pairs(Agents, _, Pairs),
                member(_-EdibleAgent, Pairs)
            ),
            FilteredAgents),
    findall([X, Y],                      % Template for output list elements
            (member(Object, FilteredAgents),    % Iterate over each object in the list
             get_dict(x, Object, X),     % Retrieve the X coordinate
             get_dict(y, Object, Y)      % Retrieve the Y coordinate
            ),
            Coordinates_temp),
    % writeln(X1),
    % writeln(Y1),
    calculate_distances([X1,Y1],Coordinates_temp,Distances),
    % writeln(Distances),
    min_nonzero_index(Distances,Index),
    IndexList is Index - 1,
    % write('Smallest Index: '),
    % writeln(IndexList),
    get_dict(IndexList, Agents, NearestAgent),
    % writeln(Nearest),
    get_dict(x, NearestAgent, X_N),
    get_dict(y, NearestAgent, Y_N),
    % write('Smallest Index Coordinates: '),
    % write(X_N),
    % write(','),
    % writeln(Y_N),
    create_coordinates(X_N,Y_N,Coordinates).
create_coordinates(X, Y, Coordinates) :-
    Coordinates = [X, Y].
round(X,Y,D) :- 
Z is X * 10^D,
 round(Z, ZA),
Y is ZA / 10^D.
calculate_distances(_, [], []).
calculate_distances([X1, Y1], [[X2, Y2]|RestCoordinates], [Distance|RestDistances]) :-
    DistX is X2 - X1,
    DistY is Y2 - Y1,
    round(sqrt(DistX * DistX + DistY * DistY),Distance,1),
    calculate_distances([X1, Y1], RestCoordinates, RestDistances).

% Predicate to find the index of the smallest nonzero number in a list
% Call with initial values: find_smallest_nonzero(List, 1, 0, 0, Index).
% Predicate to find the index of the smallest nonzero number in a list.
% It calls min_nonzero_index/4 with an initial index of 1 and no current minimum found.

% Predicate to find the index of the smallest nonzero number in a list.
% Calls min_nonzero_index/4 with an initial index of 1 and no current minimum found.

min_nonzero_index(List, Index) :-
    min_nonzero_index(List, 1, inf, 0, Index).

% Base case: When we finish the list, we check if a minimum has been found.
min_nonzero_index([], _, _, MinIndex, MinIndex).

% Recursive case: Skip zero elements.
min_nonzero_index([0|Tail], CurrentIndex, CurrentMin, MinIndex, Result) :-
    NextIndex is CurrentIndex + 1,
    min_nonzero_index(Tail, NextIndex, CurrentMin, MinIndex, Result).

% Recursive case: Process non-zero elements.
min_nonzero_index([Head|Tail], CurrentIndex, CurrentMin, MinIndex, Result) :-
    Head \= 0,
    update_min(Head, CurrentIndex, CurrentMin, MinIndex, NewMin, NewMinIndex),
    NextIndex is CurrentIndex + 1,
    min_nonzero_index(Tail, NextIndex, NewMin, NewMinIndex, Result).

% Helper predicate to update the minimum value and index.
update_min(Value, Index, CurrentMin, _, Value, Index) :-
    Value < CurrentMin.
update_min(Value, _, CurrentMin, CurrentMinIndex, CurrentMin, CurrentMinIndex) :-
    Value >= CurrentMin.


find_nearest_food(State, AgentId, Coordinates, FoodType, Distance):-
    state(Agents, Objects, _, _),
    get_dict(AgentId, Agents, Agent),
    get_dict(x, Agent, X1),
    get_dict(y, Agent, Y1),
    get_dict(subtype,Agent,EaterType),
    find_food_coordinates_with_types(State, AgentId,AgentFood,ObjectFood,FoodCoordinates),
    calculate_distances([X1,Y1],AgentFood,AgentFoodDistances),
    calculate_distances([X1,Y1],ObjectFood,ObjectFoodDistances),
    find_absolute_minimum(AgentFoodDistances, ObjectFoodDistances, Min, MinIndex, ListNum),
    extract_coordinates(ListNum, MinIndex, AgentFood, ObjectFood, NearestFoodX, NearestFoodY),
    create_coordinates(NearestFoodX, NearestFoodY, Coordinates).
    % Predicate to extract coordinates based on the list identity and index

extract_coordinates(ListNum, MinIndex, AgentFood, ObjectFood, NearestFoodX, NearestFoodY) :-
    (   ListNum = 1
    ->  nth1(MinIndex, AgentFood, Coordinates),
        nth1(1, Coordinates, NearestFoodX),
        nth1(2, Coordinates, NearestFoodY)
    ;   nth1(MinIndex, ObjectFood, Coordinates),
        nth1(1, Coordinates, NearestFoodX),
        nth1(2, Coordinates, NearestFoodY)
            ).

    
% Predicate to merge two lists with index and origin tagging
merge_tagged(List1, List2, Merged) :-
    tag_list(List1, 1, 1, Tagged1),
    tag_list(List2, 1, 2, Tagged2),
    append(Tagged1, Tagged2, Merged).

% Helper to tag elements of the list with their index and origin
tag_list([], _, _, []).
tag_list([H|T], Index, Tag, [tag(H, Index, Tag)|TaggedT]) :-
    NextIndex is Index + 1,
    tag_list(T, NextIndex, Tag, TaggedT).

% Find the minimum by absolute value in a tagged list
find_min_abs([tag(H, I, T)], H, I, T).
find_min_abs([tag(H, I, T)|Tl], Min, MinIndex, Tag) :-
    find_min_abs(Tl, CurrMin, CurrMinIndex, CurrTag),
    (   abs(H) < abs(CurrMin)
    ->  Min = H, MinIndex = I, Tag = T
    ;   Min = CurrMin, MinIndex = CurrMinIndex, Tag = CurrTag
    ).

% Main predicate to find absolute minimum across two lists
find_absolute_minimum(List1, List2, Min, MinIndex, ListNum) :-
    merge_tagged(List1, List2, Merged),
    find_min_abs(Merged, Min, MinIndex, ListNum).

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)


