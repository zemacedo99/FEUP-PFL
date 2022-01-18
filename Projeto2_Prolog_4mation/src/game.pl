:- consult('utils.pl').

:- use_module(library(random)).

game_cycle(GameState-Player,_):-
    game_over(GameState-Player, Winner),
    cls,
    display_game(GameState),
    congratulate(Winner).

game_cycle(GameState-Player,Level):-
    cls,
    display_game(GameState-Player),
    check_choice(GameState-Player,Level,RowIndex-PositionIndex),
    move(GameState-Player, RowIndex-PositionIndex, NewGameState),
    next_player(Player, NextPlayer),
    game_cycle(NewGameState-NextPlayer,Level).


not_pc_mode(Player):-
    Player \= 'PC',
    Player \= 'PC1',
    Player \= 'PC2'.

not_human_mode(Player):-
    Player \= 'Human',
    Player \= 1,
    Player \= 2.

% choose_move(+GameState, +Level, -Move).

choose_move(_-Player,_,RowIndex-PositionIndex):-
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

choose_move(GameState-Player,Level,RowIndex-PositionIndex):-
    not_human_mode(Player),
    valid_moves(GameState-Player, Moves),
    choose_move(GameState, computer-Level, RowIndex-PositionIndex, Moves).


choose_move(_,computer-1, RowIndex-PositionIndex, Moves):-
    random_select(RowIndex-PositionIndex, Moves, _).
    % length(GameState,Length),
    % random(0,Length,RowIndex), 
    % random(0,Length,PositionIndex).

choose_move(GameState,computer-_, RowIndex-PositionIndex, Moves):-
    % setof(Value-Mv, NewState^( member(Mv, Moves),
    % move(GameState, Mv, NewState),
    % evaluate_board(NewState, Value) ), [_V-Move|_]).
    choose_move(GameState, computer-1, RowIndex-PositionIndex, Moves).
    
check_choice(GameState-Player,Level,RowIndex-PositionIndex):-
    choose_move(GameState-Player,Level,RowIndex-PositionIndex),
    valid_move(GameState-Player,RowIndex-PositionIndex),
    wait_menu.

check_choice(GameState-Player,Level,RowIndex-PositionIndex):-
    check_choice(GameState-Player,Level,RowIndex-PositionIndex).

% TODO: valid_moves(+GameState, -ListOfMoves)
valid_moves(GameState-Player, Moves):-
    findall(RowIndex-PositionIndex, valid_move(GameState-Player, RowIndex-PositionIndex), Moves).


valid_move(GameState-Player,RowIndex-PositionIndex):-
    valid_bounds(GameState-Player,RowIndex-PositionIndex),
    valid_empty_positon(GameState-Player,RowIndex-PositionIndex).
    % TODO: Validate if the position is adjacent (othogonally or diagonally) to the last position your opponent played 

valid_bounds(GameState-_,RowIndex-PositionIndex):-
    length(GameState,Length),
    NewLength is Length - 1,
    between(0,NewLength,RowIndex),
    between(0,NewLength,PositionIndex).


valid_bounds(GameState-Player,_):-
    not_pc_mode(Player),
    cls,
    display_game(GameState-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----   Option out of bounds  -----\n'),
    write('-----       pls try again     -----\n'),
    write('-----                         -----\n'),
    fail.

valid_bounds(_,_):-fail.

valid_empty_positon(GameState-_,RowIndex-PositionIndex):-
    getRow(RowIndex, GameState, Row),
    getPosition(PositionIndex, Row, Position),
    Position == ' '.

valid_empty_positon(GameState-Player,_):-
    not_pc_mode(Player),
    cls,
    display_game(GameState-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('----- The position you chose  -----\n'),
    write('-----       is not empty      -----\n'),
    write('-----                         -----\n'),
    fail.

valid_empty_positon(_,_):-fail.

next_player(1, 2).
next_player(2, 1).
next_player('PC', 'Human').
next_player('Human', 'PC').
next_player('PC1', 'PC2').
next_player('PC2', 'PC1').

% move(+GameState, +Move, -NewGameState)

move(GameState-Player, RowIndex-PositionIndex, NewGameState):-
    replace_row(GameState-Player, RowIndex-PositionIndex, NewGameState).

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
    halt.

% TODO:game_over(+GameState, -Winner)

game_over(GameState-Player, Winner):-
    valid_moves(GameState-Player, Moves),
    length(Moves,Length),
    Length == 0,
    next_player(Player,NextPlayer),
    Winner = NextPlayer.

congratulate(Winner):-
    write('\nCongrats Player '),
    write(Winner),
    write('\n\n\n').

% TODO:value(+GameState, +Player, -Value) (Aqui ou no GameState.pl?)
