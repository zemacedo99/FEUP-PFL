# TP2 PFL - 4Mation

## Group 4Mation_3

| Name                                                     | Number    | E-Mail               |
| -------------------------------------------------------- | --------- | -------------------- |
| [Deborah Marques Lago](mailto:@up.pt)                    | 201806102 | up201806102@fe.up.pt |
| [José António Dantas Macedo](mailto:@up.pt)              | 201705226 | up201705226@fe.up.pt |
| [Tiago Filipe Lima Rocha](mailto:@up.pt)                 | 201406679 | up201406679@fe.up.pt |

## Installation and Execution

  Put all files with extension '.pl' in the same folder.
  Start Sicstus Prolog (or another Prolog development environment compatible with Sicstus and the ISO standard).

  Consult / import the file '4Mation.pl' *<consult (' path / to / file / 4Mation.pl ')>* .
  
  Call the play/0 function by typing the command 'play.'.
  Follow the game instructions.

## Game Description

4Mation is a 2-player game by Melanie Haumer.

The aim is to strategically place cubes in your colour to create a horizontal, vertical or diagonal row of 4.

*Gameplay*
The players take turns to play one of their cubes. Important: Each cube can only be placed in a space that is adjacent (othogonally or diagonally) to the last cube, your opponent played!

*Goal*
The game ends as soon as one player has at least 4 cubes in their colour in a horizontal, vertical or diagonal row.
That player is the winner. If neither player is able to complete a row of 4, the game ends in a draw.

*Game Components*
![Game Components][ref]

[ref]: ./assets/game.jpg "4Mation Components"

The board game version of 4Mation have:
1 board with a shape of a 7x7 square and 48 pieces (24 blue cubes, 24 pink cubes).

You can find a more explanation online:
[BoardGameGeek](https://boardgamegeek.com/boardgame/329175/4mation).
[4MationVideo](https://www.youtube.com/watch?v=KFeCxg7BWhM).

## Game Logic

Program starts by calling the predicate **play/0**, in which the initial menu is displayed calling **display_main_menu/0**.

### Internal representation of the state of the game

### Visualizing the game

### Execution of a move

### End of the game

### List of valid plays

### Game status evaluation*

### Computer Play*

## Conclusions

## Bibliography

[More information about the game 4Mation](https://boardgamegeek.com/boardgame/329175/4mation)

[Prolog Manual](https://www.swi-prolog.org/pldoc/doc_for?object=manual)
