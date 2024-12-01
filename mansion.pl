% Define the mansion layout
room(kitchen).
room(library).
room(ballroom).
room(conservatory).
room(dining_room).
room(hall).
room(lounge).
room(study).

% Define adjacency between rooms
adjacent(kitchen, ballroom).
adjacent(ballroom, library).
adjacent(library, conservatory).
adjacent(conservatory, dining_room).
adjacent(dining_room, hall).
adjacent(hall, lounge).
adjacent(lounge, study).

% Helper to check if a room is adjacent
is_adjacent(Room1, Room2) :- adjacent(Room1, Room2); adjacent(Room2, Room1).

