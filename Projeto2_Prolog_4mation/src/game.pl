:- consult('utils.pl').

:- use_module(library(random)).

% display_game(+GameState)
% initial_state(+Size, -GameState)
% move(+GameState, +Move, -NewGameState)

% move(GameState, Move, NewGameState):-
%     write(Move).

% game_over(+GameState, -Winner)
% valid_moves(+GameState, -ListOfMoves) (Aqui ou no GameState.pl?)

game_cycle(GameState-Player,Level):-
    cls,
    display_game(GameState-Player),
    check_choice(GameState-Player,Level,RowIndex-PositionIndex),
    move(GameState-Player, RowIndex-PositionIndex, NewGameState),
    next_player(Player, NextPlayer),
    game_cycle(NewGameState-NextPlayer,Level).

% game_cycle(GameState-Player):-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).

not_pc_mode(Player):-
    Player \= 'PC',
    Player \= 'PC1',
    Player \= 'PC2'.

not_human_mode(Player):-
    Player \= 'Human',
    Player \= 1,
    Player \= 2.

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
    % valid_moves(GameState-Player, Moves),
    choose_move(GameState, computer-Level, RowIndex-PositionIndex).


choose_move(GameState,computer-1, RowIndex-PositionIndex):-
    length(GameState,Length),
    random(0,Length,RowIndex), 
    random(0,Length,PositionIndex).

choose_move(GameState,computer-_, RowIndex-PositionIndex):-
    % setof(Value-Mv, NewState^( member(Mv, Moves),
    % move(GameState, Mv, NewState),
    % evaluate_board(NewState, Value) ), [_V-Move|_]).
    choose_move(GameState, 1, RowIndex-PositionIndex).
    
check_choice(GameState-Player,Level,RowIndex-PositionIndex):-
    choose_move(GameState-Player,Level,RowIndex-PositionIndex),
    valid_move(GameState-Player,RowIndex-PositionIndex),
    wait_menu.

check_choice(GameState-Player,Level,RowIndex-PositionIndex):-
    check_choice(GameState-Player,Level,RowIndex-PositionIndex).

valid_moves(GameState-Player, Moves):-
    findall(RowIndex-PositionIndex, move(GameState-Player, RowIndex-PositionIndex, _), Moves).


valid_move(GameState-Player,RowIndex-PositionIndex):-
    valid_bounds(GameState-Player,RowIndex-PositionIndex),!,
    valid_empty_positon(GameState-Player,RowIndex-PositionIndex).
    % TODO: Validate if the position is adjacent (othogonally or diagonally) to the last position your opponent played 

valid_bounds(GameState-_,RowIndex-PositionIndex):-
    length(GameState,Length),
    RowIndex >= 0,
    PositionIndex >= 0,
    RowIndex < Length,
    PositionIndex < Length.

valid_bounds(GameState-Player,_):-
    cls,
    display_game(GameState-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----   Option out of bounds  -----\n'),
    write('-----       pls try again     -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    fail.

valid_empty_positon(GameState-_,RowIndex-PositionIndex):-
    getRow(RowIndex, GameState, Row),
    getPosition(PositionIndex, Row, Position),
    Position == ' '.

valid_empty_positon(GameState-Player,_):-
    cls,
    display_game(GameState-Player),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('----- The position you chose  -----\n'),
    write('-----       is not empty      -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    fail.

next_player(1, 2).
next_player(2, 1).
next_player('PC', 'Human').
next_player('Human', 'PC').
next_player('PC1', 'PC2').
next_player('PC2', 'PC1').

move(GameState-Player, RowIndex-PositionIndex, NewGameState):-
    % valid_move(GameState-Player,RowIndex-PositionIndex),
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






% value(+GameState, +Player, -Value) (Aqui ou no GameState.pl?)
% choose_move(+GameState, +Level, -Move).

% GameMode
%   pvc / cvp
%   pvp
%   cvc

% Difficulty Level
%   Level1: random valid move
%   Level2: best move (greedy/blind algorithm)