% Define characters
character(miss_scarlett).
character(colonel_mustard).
character(mrs_white).
character(mr_green).
character(mrs_peacock).
character(professor_plum).

% Define weapons
weapon(candlestick).
weapon(revolver).
weapon(rope).
weapon(knife).
weapon(lead_pipe).
weapon(wrench).

% Define player starting positions
start_position(miss_scarlett, lounge).
start_position(colonel_mustard, kitchen).
start_position(mrs_white, ballroom).
start_position(mr_green, conservatory).
start_position(mrs_peacock, library).
start_position(professor_plum, study).

% Randomly select the solution
:- dynamic solution/3.
generate_solution :-
    findall(X, character(X), Characters),
    findall(Y, weapon(Y), Weapons),
    findall(Z, room(Z), Rooms),
    random_member(Character, Characters),
    random_member(Weapon, Weapons),
    random_member(Room, Rooms),
    assert(solution(Character, Weapon, Room)).
