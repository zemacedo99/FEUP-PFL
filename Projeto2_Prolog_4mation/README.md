# TP2 PFL - 4Mation

## Group 4Mation_3

| Name                                                                | Number    | E-Mail               |
| ------------------------------------------------------------------- | --------- | -------------------- |
| [Deborah Marques Lago](mailto:up201806102@up.pt)                    | 201806102 | up201806102@fe.up.pt |
| [José António Dantas Macedo](mailto:up201705226@up.pt)              | 201705226 | up201705226@fe.up.pt |
| [Tiago Filipe Lima Rocha](mailto:up201406679@up.pt)                 | 201406679 | up201406679@fe.up.pt |

## Installation and Execution

  Put all files with extension '.pl' in the same folder.
  Start Sicstus Prolog (or another Prolog development environment compatible with Sicstus and the ISO standard).

  Consult / import the file '4Mation.pl' *<consult (' path / to / file / 4Mation.pl ')>* .
  
  Call the **play/0** function by typing the command 'play.'.
  Follow the game instructions.

## Game Description

4Mation is a 2-player game made by Melanie Haumer.

The aim is to strategically place your pieces (cubes with the your color) on the board, in order create a horizontal, vertical or diagonal row of 4 before the opponent has the chance to.

*Gameplay*

The players take turns to play one of their cubes. *Important*: Each cube can only be placed in a space that is adjacent (orthogonally or diagonally) to the last cube, your opponent played!

*Goal*

The game ends as soon as one player has at least 4 cubes in their colour in a horizontal, vertical or diagonal row.
That player is the winner. If neither player is able to complete a row of 4, and there's no valid moves left, the game ends in a draw.

*Game Components*
![Game Components][ref]

[ref]: ./report/assets/game.jpg "4Mation Components"

The board game version of 4Mation have:
1 board with a shape of a 7x7 square and 48 pieces (24 blue cubes, 24 pink cubes).

You can find a more complete explanation online:
[BoardGameGeek](https://boardgamegeek.com/boardgame/329175/4mation).
[4MationVideo](https://www.youtube.com/watch?v=KFeCxg7BWhM).

## Game Logic

The game start with the predicate **play/0**.

### Internal representation of the state of the game

The Game State is composed by the board, the current player and the position of the last move (row and column indexes).
A Board is a list of lists, where the atoms are empty strings when the game starts. The Players are identified by their Pieces, which can be one of the chars 'O' and 'X', so when there's a move, these chars substitute an empty position on the board.

```
initial_state(0,_).

initial_state(Size,Board):-
    Piece = ' ',
    NewSize is Size - 1,
    make_row(NewSize,Piece,[Piece],Row),
    make_board(NewSize,Row,[Row],Board).

make_board(0,_,GameState,Board).

make_board(Size,Row,CurrentBoard,Board):-
    append(CurrentBoard,[Row], NewBoard),
    NewSize is Size - 1,
    make_board(NewSize,Row,NewBoard,Board).

make_row(0,_,Row,Row).

make_row(Size,Piece,CurrentRow,Row):-
    append(CurrentRow,[Piece], NewRow),
    NewSize is Size - 1,
    make_row(NewSize,Piece,NewRow,Row).
```
*Start Game*
![Start Game][ref2]

[ref2]: ./report/assets/startBoard.png "Start Board"

*Intermediate Game*
![Intermediate Game][ref3]

[ref3]: ./report/assets/intermediateBoard.png "Intermediate Board"

*Final Game*
![Final Game][ref4]

[ref4]: ./report/assets/finalBoard.png "Final Board"

### Visualizing the game

The **display_game/1** predicate receives either the compound term Board-Player (for all intermediate and final states, the current Board plus the Player that is going to play next) or just the Board (for the initial state). We use some auxiliar predicates for printing the parts of the game: **print_line/1**, **print_rows/2**, **print_column_number/1** and **print_player_piece/1**. However, in one of the initial menus, the user is asked to insert the board size he/she wants. So, this input is used as the parameter in the predicate **initial_state(+Size, -GameState)** which returns the initial GameState, that is, just an empty board in the chosen size.
All the user interactions are made through either menus or messages. In order to validate the user inputs, the menus run in constant loop until a valid choice is made. If not the case, the user will see a message telling him/her it wasn't a valid option and then be asked for another input.

*Main Menu*

![Start Menu][ref5]

[ref5]: ./report/assets/mainMenu.png "Start Menu"

*Level Menu*

![Level Menu][ref6]

[ref6]: ./report/assets/levelMenu.png "Level Menu"

*Board Size Menu*

![Board Size Menu][ref7]

[ref7]: ./report/assets/sizeMenu.png "Size Menu"

*Choosing Position Menus*

![Choosing Position Menus][ref8]

[ref8]: ./report/assets/chooseRowSpace.png "Choosing Row Menu"


### Execution of a move

During the game cycle, our predicate **check_choice/3** is constantly checking if a certain move is valid and if it is, it then calls the predicate **choose_move/3** which behaves differently depending if the current Player is Human or the PC. When Human, it will ask for the user's input (Row and then Column). For the PC Player, it will choose a one of the moves from the list generated by the **valid_moves/2** predicate.
After the verifications are made and the move is chosen, the **move/3** predicate executes the move, returning the updated Game State.

### End of the game

The game cycle is always verifying if game over conditions are being met through calling the **game_over/2** predicate, which will either find four pieces in a row and return the Winner, verify that there are no valid moves left and it is a draw or none of them. In this last case, it will fail the first override of *game_cycle/2* predicate and continue to the other version, where the game continues.

```
game_over(Board-_-_-_, Winner):-
    four_in_a_row(Board,Winner).

game_over(Board-_-LastRowIndex-LastPositionIndex, Winner):-
    valid_moves(Board-'PC'-LastRowIndex-LastPositionIndex, Moves),
    length(Moves,Length),
    Length == 0,
    Winner = 'draw'.

congratulate(Winner):-
    Winner == 'draw',
    write('\nIt\'s a '),
    write(Winner),
    write('\n\n\n').

congratulate(Winner):-
    write('\nCongrats Player '),
    write(Winner),
    write('\n\n\n').
```


### List of valid plays

In order to choose the Computer move in every turn, we generate a list of all the possible moves in a determinate game state using the superior order function *findall/3*. This function verifies if a certain position is valid: if it's inside bounds, if it's an empty space and if it's adjacent to the last piece placed by the other player. If all the conditions are met, this position is inserted in the list Moves.

```
valid_moves(Board-Player-LastRowIndex-LastPositionIndex, Moves):-
    findall(RowIndex-PositionIndex, valid_move(Board-Player-LastRowIndex-LastPositionIndex, RowIndex-PositionIndex), Moves).
```

### Computer Play

When it is the computer's turn to play, the predicate *valid_moves/2* will generate a list of possible moves and then the override of *choose_move/3* will call the *random_select/3* function returning a random move from the list of possible moves.

```
choose_move(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex):-
    not_human_mode(Player),
    valid_moves(Board-Player-LastRowIndex-LastPositionIndex, Moves),
    choose_move(Board, computer-Level, RowIndex-PositionIndex, Moves).

choose_move(_,computer-1, RowIndex-PositionIndex, Moves):-
    random_select(RowIndex-PositionIndex, Moves, _).
```

## Conclusions

This project was developed in Prolog language within PFL subject at FEUP.

We implemented the board game 4Mation following the specs of the [project guideline](https://moodle.up.pt/pluginfile.php/141752/mod_resource/content/1/TP2%20-%20Enunciado.pdf). From the specs required, we manage to fulfill them all, except from the implementation of the board evaluation predicate and the computer difficulty level 2 due to the lack of time.

Besides that, the project is well structured and organized, it deals with wrong inputs from users, any board size (including adaptation of the winning rule depending on the board size. Example: in 3x3 boards, the winner needs 2 in a row to win and so on), and all the game modes (Human vs Human, Human vs PC, PC vs Human, PC vs PC).

We believe that we have developed an excellent project and we do hope you enjoy the game.

## Bibliography

[More information about the game 4Mation](https://boardgamegeek.com/boardgame/329175/4mation)

[Prolog Manual](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
