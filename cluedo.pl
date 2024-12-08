:- [mansion].
:- [characters_and_weapons].
:- [gameplay].

% Start the game
start_game :- 
    generate_solution,
    write('Welcome to Cluedo!'), nl,
    write('The mansion is ready, and the investigation begins!'), nl,
    setup_players,
    write('Players have been placed in their starting rooms.'), nl,
    write('You can suggest at any time, and check the solution when needed.'), nl,
    play.

% Setup players randomly
setup_players :-
    findall(Character, character(Character), Characters),
    findall(Room, room(Room), Rooms),
    randomize_players(Characters, Rooms).

% Randomly assign players to rooms (ensures unique assignment)
randomize_players([], _).
randomize_players([Character|T], Rooms) :-
    random_member(Room, Rooms),
    init_player(Character, Room),
    delete(Rooms, Room, RemainingRooms),
    randomize_players(T, RemainingRooms).

% Main gameplay loop (no turn-wise restriction)
play :-
    repeat,
    write('Enter your command: '), nl,
    read(Command),
    process_command(Command),
    (Command == quit -> ! ; fail).

% Process user commands
process_command(move(Player, Room)) :-
    move_player(Player, Room).  % Ensure this calls the correct predicate
process_command(suggest(Player, Room, Suspect, Weapon)) :-
    suggest(Player, Room, Suspect, Weapon).

process_command(positions) :- 
    display_positions.
process_command(current_position_and_moves(Player)) :- 
    current_position_and_moves(Player).
process_command(display_solution) :- 
    display_solution.
process_command(quit) :- 
    write('Thank you for playing Cluedo! Goodbye!'), nl.
process_command(_) :- 
    write('Unknown command! Type "instructions." for help.'), nl.

% Display the solution
display_solution :- 
    solution(Suspect, Weapon, Room),
    write('The solution to the mystery is: '), nl,
    write('Suspect: '), write(Suspect), nl,
    write('Weapon: '), write(Weapon), nl,
    write('Room: '), write(Room), nl.

