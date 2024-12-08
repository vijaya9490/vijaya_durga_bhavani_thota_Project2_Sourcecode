:- [mansion].
:- [characters_and_weapons].
:- [gameplay].


% Start the interactive game
interactive_game :- 
    start_turn_based_game,
    display_instructions,
    game_loop.

% Display game instructions
display_instructions :-
    nl,
    write('Welcome to the Interactive Cluedo Game!'), nl,
    write('Your goal is to solve the mystery: Who committed the murder, with what weapon, and in which room?'), nl,
    write('Commands you can use:'), nl,
    write('- move(Player, Room): Move a player to an adjacent room.'), nl,
    write('- suggest(Player, Room, Suspect, Weapon): Make a suggestion about the murder.'), nl,
    write('- positions: Display the current positions of all players.'), nl,
    write('- current_position_and_moves(Player): Show current room and valid moves.'), nl,
    write('- instructions: Display these instructions again.'), nl,
    write('- display_solution: Display the solution of the mystery.'), nl,
    write('- quit: End the game.'), nl,
    write('Tip: Use `current_position_and_moves(Player)` to plan your moves.'), nl.

% Initialize the game
start_turn_based_game :-
    generate_solution,
    setup_players,
    write('Game initialized. The mystery begins!'), nl.

% Setup players randomly
setup_players :-
    findall(Character, character(Character), Characters),
    findall(Room, room(Room), Rooms),
    randomize_players(Characters, Rooms),
    write('Players have been placed in their starting rooms.'), nl.

% Randomly assign players to rooms
randomize_players([], _).
randomize_players([Character|T], Rooms) :-
    random_member(Room, Rooms),
    init_player(Character, Room),
    randomize_players(T, Rooms).

% Game loop
game_loop :-
    repeat,
    write('Enter your command: '), nl,
    read(Command),
    process_command(Command),
    (Command == quit -> ! ; fail).

% Process user commands
process_command(move(Player, Room)) :-
    move_player(Player, Room).
process_command(suggest(Player, Room, Suspect, Weapon)) :-
    suggest(Player, Room, Suspect, Weapon).

process_command(positions) :- 
    display_positions.
process_command(current_position_and_moves(Player)) :-
    current_position_and_moves(Player).
process_command(instructions) :- display_instructions.
process_command(display_solution) :- display_solution.  % Added here
process_command(quit) :- write('Thank you for playing Cluedo! Goodbye!'), nl.
process_command(_) :- write('Unknown command! Type "instructions." for help.'), nl.

% Display current player positions
display_positions :-
    findall((Player, Room), player_position(Player, Room), Positions),
    (   Positions == [] 
    ->  write('No players are initialized yet.'), nl
    ;   write('Current player positions: '), nl,
        write(Positions), nl).

% Display the solution
display_solution :- 
    solution(Suspect, Weapon, Room),
    write('The solution to the mystery is: '), nl,
    write('Suspect: '), write(Suspect), nl,
    write('Weapon: '), write(Weapon), nl,
    write('Room: '), write(Room), nl.

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
        write('Room: '), write(SolutionRoom), nl,
        write('Congratulations! You have solved the mystery!'), nl,
        retractall(solution(_, _, _))  % End the game by removing the solution
    ;   write('To Start New game use command : interactive_game.'), nl).
