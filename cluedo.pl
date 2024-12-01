:- [mansion].
:- [characters_and_weapons].
:- [gameplay].

% Start the game
start_game :-
    generate_solution,
    write('Welcome to Cluedo!'), nl,
    write('The mansion is ready, and the investigation begins!'), nl,
    setup_players.

% Setup players
setup_players :-
    findall(Character, character(Character), Characters),
    forall(member(C, Characters), (start_position(C, Room), init_player(C, Room))),
    write('Players have been placed in their starting rooms.'), nl.

% Main gameplay loop
play :-
    repeat,
    write('Enter your move (format: move_player(Player, Room).): '), nl,
    read(Command),
    (   Command == quit
    ->  write('Thanks for playing Cluedo!'), nl, !
    ;   call(Command), fail
    ).
