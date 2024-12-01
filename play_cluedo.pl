:- [mansion].
:- [characters_and_weapons].
:- [gameplay].

% Start the interactive game
interactive_game :-
    start_game,
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
    write('- quit: End the game.'), nl,
    write('Tip: Use `current_position_and_moves(Player)` to plan your moves.'), nl.

% Initialize the game
start_game :-
    generate_solution,
    write('Game initialized. The mystery begins!'), nl,
    setup_players.

% Setup players
setup_players :-
    findall(Character, character(Character), Characters),
    forall(member(C, Characters), (start_position(C, Room), init_player(C, Room))),
    write('Players placed in their starting positions.'), nl.

% Game loop
game_loop :-
    repeat,
    write('Enter your command: '), nl,
    read(Command),
    process_command(Command),
    (Command == quit -> ! ; fail).

% Process user commands
process_command(move(Player, Room)) :- move_player(Player, Room).
process_command(suggest(Player, Room, Suspect, Weapon)) :- suggest(Player, Room, Suspect, Weapon).
process_command(positions) :-
    findall((Player, Room), player_position(Player, Room), Positions),
    write('Current player positions: '), nl,
    write(Positions), nl.
process_command(current_position_and_moves(Player)) :- current_position_and_moves(Player).
process_command(instructions) :- display_instructions.
process_command(quit) :- write('Thank you for playing Cluedo! Goodbye!'), nl.
process_command(_) :- write('Unknown command! Type "instructions." for help.'), nl.
