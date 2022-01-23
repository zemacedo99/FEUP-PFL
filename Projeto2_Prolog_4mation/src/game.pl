:- consult('utils.pl').

:- use_module(library(random)).

game_cycle(Board-Player-LastRowIndex-LastPositionIndex,_):-
    game_over(Board-Player-LastRowIndex-LastPositionIndex, Winner),
    cls,
    display_game(Board),
    congratulate(Winner).

game_cycle(Board-Player-LastRowIndex-LastPositionIndex,Level):-
    cls,
    display_game(Board-Player),
    check_choice(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex),
    move(Board-Player, RowIndex-PositionIndex, NewBoard),
    next_player(Player, NextPlayer),
    game_cycle(NewBoard-NextPlayer-RowIndex-PositionIndex,Level).


not_pc_mode(Player):-
    Player \= 'PC',
    Player \= 'PC1',
    Player \= 'PC2'.

not_human_mode(Player):-
    Player \= 'Human',
    Player \= 1,
    Player \= 2.

not_player_two(Player):-
    Player \= 2,
    Player \= 'PC',
    Player \= 'PC2'.

not_player_one(Player):-
    Player \= 1,
    Player \= 'Human',
    Player \= 'PC1'.

% choose_move(+Board, +Level, -Move).

choose_move(_-Player-_-_,_,RowIndex-PositionIndex):-
    not_pc_mode(Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----    Chose a Row to play  -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read(RowIndex),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----    Chose a empty space  -----\n'),
    write('-----          to play        -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read(PositionIndex).

choose_move(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex):-
    not_human_mode(Player),
    valid_moves(Board-Player-LastRowIndex-LastPositionIndex, Moves),
    choose_move(Board, computer-Level, RowIndex-PositionIndex, Moves).

choose_move(_,computer-1, RowIndex-PositionIndex, Moves):-
    random_select(RowIndex-PositionIndex, Moves, _).

% TODO: computer level 2
choose_move(Board,computer-_, RowIndex-PositionIndex, Moves):-
    choose_move(Board, computer-1, RowIndex-PositionIndex, Moves).
    
check_choice(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex):-
    choose_move(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex),
    valid_move(Board-Player-LastRowIndex-LastPositionIndex,RowIndex-PositionIndex),
    wait_menu.

check_choice(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex):-
    check_choice(Board-Player-LastRowIndex-LastPositionIndex,Level,RowIndex-PositionIndex).

% valid_moves(+Board, -ListOfMoves)
valid_moves(Board-Player-LastRowIndex-LastPositionIndex, Moves):-
    findall(RowIndex-PositionIndex, valid_move(Board-Player-LastRowIndex-LastPositionIndex, RowIndex-PositionIndex), Moves).


valid_move(Board-Player-LastRowIndex-LastPositionIndex,RowIndex-PositionIndex):-
    valid_bounds(Board-Player,RowIndex-PositionIndex),
    valid_empty_positon(Board-Player,RowIndex-PositionIndex),
    valid_adjacent(Board-Player-LastRowIndex-LastPositionIndex,RowIndex-PositionIndex).

valid_bounds(Board-Player,RowIndex-PositionIndex):-
    not_pc_mode(Player),
    length(Board,Length),
    NewLength is Length - 1,
    between(0,NewLength,RowIndex),
    between(0,NewLength,PositionIndex),!.

valid_bounds(Board-Player,RowIndex-PositionIndex):-
    not_human_mode(Player),
    length(Board,Length),
    NewLength is Length - 1,
    between(0,NewLength,RowIndex),
    between(0,NewLength,PositionIndex).


valid_bounds(Board-Player,_):-
    not_pc_mode(Player),
    cls,
    display_game(Board-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----   Option out of bounds  -----\n'),
    write('-----       pls try again     -----\n'),
    write('-----                         -----\n'),
    fail,!.

valid_bounds(_,_):-fail,!.

valid_empty_positon(Board-_,RowIndex-PositionIndex):-
    getRow(RowIndex, Board, Row),
    getPosition(PositionIndex, Row, Position),
    Position == ' ',!.

valid_empty_positon(Board-Player,_):-
    not_pc_mode(Player),
    cls,
    display_game(Board-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('----- The position you chose  -----\n'),
    write('-----       is not empty      -----\n'),
    write('-----                         -----\n'),
    fail,!.

valid_empty_positon(_,_):-fail,!.


valid_adjacent(_-_-LastRowIndex-LastPositionIndex,_):-
    LastRowIndex == 'first_move',
    LastPositionIndex == 'first_move',!.

valid_adjacent(_-_-LastRowIndex-LastPositionIndex,RowIndex-PositionIndex):-
    CheckTop = LastRowIndex + 1,
    CheckBottom = LastRowIndex - 1,
    CheckRight = LastPositionIndex + 1,
    CheckLeft = LastPositionIndex - 1,
    RowIndex =< CheckTop,
    RowIndex >= CheckBottom,
    PositionIndex =< CheckRight,
    PositionIndex >= CheckLeft,!.

valid_adjacent(Board-Player-_-_,_):-
    not_pc_mode(Player),
    cls,
    display_game(Board-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----  The position you chose -----\n'),
    write('-----      is not adjacent    -----\n'),
    write('-----   to the last position  -----\n'),
    write('-----   your opponent played  -----\n'),
    write('-----                         -----\n'),
    fail,!.

valid_adjacent(_,_):-fail,!.

next_player(1, 2).
next_player(2, 1).
next_player('PC', 'Human').
next_player('Human', 'PC').
next_player('PC1', 'PC2').
next_player('PC2', 'PC1').

% move(+Board, +Move, -NewBoard)

move(Board-Player, RowIndex-PositionIndex, NewBoard):-
    replace_row(Board-Player, RowIndex-PositionIndex, NewBoard).

wait_menu:-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----          Press:         -----\n'),
    write('-----       1 to continue     -----\n'),
    write('-----       2 to exit         -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read(Option),
    wait_menu(Option).

wait_menu(1).

wait_menu(2):- 
    write('Exiting game...\n'),
    read(_),
    halt.

% TODO:game_over(+Board, -Winner)

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

find_a_piece(Board,Length,CheckRow,CheckPos,Piece):-
    length(Board,Length),
    BoardLength is Length - 1,
    between(0,BoardLength,CheckRow),
    between(0,BoardLength,CheckPos),
    getRow(CheckRow, Board, Row),
    getPosition(CheckPos, Row, Piece).

four_in_a_row(Board,Winner):-
    find_a_piece(Board,Length,CheckRow,CheckPos,Piece),
    Piece == 'O',

    Half is Length // 2 + 1,
    all_directions_row(Board-Length,Piece,CheckRow-CheckPos,Half),
    Winner = 2.

four_in_a_row(Board,Winner):-
    find_a_piece(Board,Length,CheckRow,CheckPos,Piece),
    Piece == 'X',
    
    Half is Length // 2 + 1,
    all_directions_row(Board-Length,Piece,CheckRow-CheckPos,Half),
    Winner = 1.

four_in_a_row(_,_):-false.

all_directions_row(Board-Length,Piece,CheckRow-CheckPos,Half):-
    CheckRight is CheckPos + 1,
    horizontal_row(Board-Length,Piece,CheckRow-CheckRight,Half);
    CheckBottom is CheckRow + 1, 
    vertical_row(Board-Length,Piece,CheckBottom-CheckPos,Half);
    CheckBottom is CheckRow + 1,
    CheckRight is CheckPos + 1,
    diagonal_row(Board-Length,Piece,CheckBottom-CheckRight,Half).

horizontal_row(_,_,_,1):-!.

horizontal_row(Board-Length,Piece,CheckRow-CheckPos,Half):-
    CheckPos < Length, 
    getRow(CheckRow, Board, Row),
    getPosition(CheckPos, Row, Position),
    Position == Piece,
    NewHalf is Half - 1,
    CheckRight is CheckPos + 1,
    horizontal_row(Board-Length,Piece,CheckRow-CheckRight,NewHalf).

horizontal_row(_,_,_,_):-fail,!.

vertical_row(_,_,_,1):-!.

vertical_row(Board-Length,Piece,CheckRow-CheckPos,Half):-
    CheckRow < Length, 
    getRow(CheckRow, Board, Row),
    getPosition(CheckPos, Row, Position),
    Position == Piece,
    NewHalf is Half - 1,
    CheckBottom is CheckRow + 1,
    vertical_row(Board-Length,Piece,CheckBottom-CheckPos,NewHalf).

vertical_row(_,_,_,_):-fail,!.

diagonal_row(_,_,_,1):-!.

diagonal_row(Board-Length,Piece,CheckRow-CheckPos,Half):-
    CheckRow < Length, 
    CheckPos < Length, 
    getRow(CheckRow, Board, Row),
    getPosition(CheckPos, Row, Position),
    Position == Piece,
    NewHalf is Half - 1,
    CheckBottom is CheckRow + 1,
    CheckRight is CheckPos + 1,
    diagonal_row(Board-Length,Piece,CheckBottom-CheckRight,NewHalf).

diagonal_row(_,_,_,_):-fail,!.

% TODO:value(+Board, +Player, -Value) (Aqui ou no Board.pl?)
