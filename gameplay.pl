:- dynamic player_position/2.

% Initialize player position
init_player(Name, StartRoom) :- assert(player_position(Name, StartRoom)).

% Move player to a new room with guidance
move_player(Player, NewRoom) :-
    player_position(Player, CurrentRoom),
    (   is_adjacent(CurrentRoom, NewRoom)
    ->  retract(player_position(Player, CurrentRoom)),
        assert(player_position(Player, NewRoom)),
        write(Player), write(' successfully moved to '), write(NewRoom), nl
    ;   write('Invalid move! '), nl,
        write('You are currently in '), write(CurrentRoom), nl,
        write('You can move to one of the following rooms: '), nl,
        findall(AdjRoom, is_adjacent(CurrentRoom, AdjRoom), AdjRooms),
        write(AdjRooms), nl).

% Display current position and valid moves for a player
current_position_and_moves(Player) :-
    player_position(Player, CurrentRoom),
    write(Player), write(' is currently in '), write(CurrentRoom), nl,
    write('You can move to the following rooms: '), nl,
    findall(AdjRoom, is_adjacent(CurrentRoom, AdjRoom), AdjRooms),
    write(AdjRooms), nl.

% Suggestion mechanism
suggest(Player, Room, Suspect, Weapon) :-
    player_position(Player, Room),
    solution(SolutionCharacter, SolutionWeapon, SolutionRoom),
    (   Suspect = SolutionCharacter,
        Weapon = SolutionWeapon,
        Room = SolutionRoom
    ->  write('Correct suggestion! Murder solved: '), nl,
        write('Suspect: '), write(SolutionCharacter), nl,
        write('Weapon: '), write(SolutionWeapon), nl,
        write('Room: '), write(SolutionRoom), nl
    ;   write('Incorrect suggestion. Keep investigating!'), nl).
