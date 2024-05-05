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
    
% 4- find_food_coordinates(+State, +AgentId, -Coordinates)

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)

% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)


