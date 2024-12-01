# Cluedo Game - Prolog

## Description
This project is a Prolog-based implementation of the classic board game **Cluedo** (or Clue). The objective is to solve the murder mystery by deducing:
1. **Who committed the murder** (Suspect)?
2. **What weapon was used** (Weapon)?
3. **In which room the murder occurred** (Room)?

The game is interactive, allowing players to move characters between rooms, make suggestions, and explore the mansion to uncover the solution.

---

## Features
- **Dynamic Solution**: The game randomly selects a suspect, weapon, and room at the start of each game.
- **Interactive Gameplay**: Players receive feedback and guidance for their actions.
- **Room Navigation**: Move players through adjacent rooms based on a predefined mansion layout.
- **Suggestions**: Propose a combination of suspect, weapon, and room to deduce the solution.
- **In-Game Help**: Access instructions at any time to guide gameplay.
- **Error Handling**: Invalid commands and moves trigger helpful feedback.

---

## Setup Instructions

### Prerequisites
- **Prolog Interpreter**: SWI-Prolog (recommended) or any compatible Prolog environment.

### Installation
1. **Clone or Download the Repository**:
   ```bash
   git clone https://github.com/vijaya9490/vijaya_durga_bhavani_thota_Project2_Sourcecode.git
2. **Navigate to code folder**:
   ```bash
   cd Studentname_Project2_SourceCode
3. **Start Prolog Interpreter**
   ```bash
   swipl
4. **Loading game file**
   ```bash
   [play_cluedo].
5. To start the game
   ```bash
   interactive_game.

### Gameplay Commands
1. **Move a player**
   Move a character to an adjacent room:
   ```bash
   move(Player, Room).
   ```
   Example:
   ```bash
   move(miss_scarlett, hall).

2. **Make a Suggestion**
   Make a guess about the suspect, weapon, and room:
   ```bash
   suggest(Player, Room, Suspect, Weapon).
   ```
   Example:
   ```bash
   suggest(miss_scarlett, ballroom, colonel_mustard, rope).
3. **To Check All Player Positions**
   View the current positions of all characters:
   ```bash
   positions.
   ```
4. **To See Current Room and Valid Moves**
   Check a player's current room and adjacent rooms:
   ```bash
   current_position_and_moves(Player).
   ```
   Example:
   ```bash
   current_position_and_moves(miss_scarlett).
5. **View Instructions**
   Display the in-game instructions:
   ```bash
   instructions.
   ```
6. **Quit the Game**
   Exit the game session:
   ```bash
   quit.
   ```

### Sample Game Execution
**Step 1: Start the Game**
After loading the game, begin the game:
```prolog
?- interactive_game.
```
Output:
```bash
Game initialized. The mystery begins!
Players placed in their starting positions.

Welcome to the Interactive Cluedo Game!
Your goal is to solve the mystery: Who committed the murder, with what weapon, and in which room?
Commands you can use:
- move(Player, Room): Move a player to an adjacent room.
- suggest(Player, Room, Suspect, Weapon): Make a suggestion about the murder.
- positions: Display the current positions of all players.
- current_position_and_moves(Player): Show current room and valid moves.
- instructions: Display these instructions again.
- quit: End the game.
Tip: Use `current_position_and_moves(Player)` to plan your moves.
Enter your command:
```

**Step 2: Check Current Position**
Find out where Miss Scarlett is and which rooms she can move to:
```prolog
?- current_position_and_moves(miss_scarlett).
```
Output:
```bash
miss_scarlett is currently in lounge
You can move to the following rooms:
[study,hall]
```

**Step 3: Move Miss Scarlett**
Move Miss Scarlett from the Lounge to the Hall:
```prolog
?- move(miss_scarlett, hall).
```
Output:
```bash
miss_scarlett successfully moved to hall
```

**Step 4: Make a Suggestion**
Propose a guess while in the Hall:
```prolog
?- suggest(miss_scarlett, hall, colonel_mustard, rope).
```
Output:
```bash
Incorrect suggestion. Keep investigating!
```

**Step 5: Check All Player Positions**
View the current positions of all characters:
```prolog
?- positions.
```

Output:
```bash
Current player positions: 
[(colonel_mustard,kitchen),(mrs_white,ballroom),(mr_green,conservatory),(mrs_peacock,library),(professor_plum,study),(miss_scarlett,hall)]
```

**Step 6: Explore Other Rooms**
Continue moving to other rooms and making suggestions:
```prolog
?- move(miss_scarlett, dining_room).
?- suggest(miss_scarlett, dining_room, mrs_peacock, candlestick).
```
Output:
```bash
Enter your command:
|: move(miss_scarlett, dining_room).
miss_scarlett successfully moved to dining_room
```
```bash
Enter your command:
|: suggest(miss_scarlett, dining_room, mrs_peacock, candlestick).
Incorrect suggestion. Keep investigating!
```
**Step 7: Quit the Game**
Exit the game:
```prolog
?- quit.
```



   
