:- consult('utils.pl').

:- use_module(library(random)).

% display_game(+GameState)
% initial_state(+Size, -GameState)
% move(+GameState, +Move, -NewGameState)

% move(GameState, Move, NewGameState):-
%     write(Move).

% game_over(+GameState, -Winner)
% valid_moves(+GameState, -ListOfMoves) (Aqui ou no GameState.pl?)

game_cycle(GameState-Player):-
    display_game(GameState-Player),
    choose_move(GameState-Player,RowIndex-PositionIndex),
    move(GameState-Player, RowIndex-PositionIndex, NewGameState),
    next_player(Player, NextPlayer),
    game_cycle(NewGameState-NextPlayer).

% game_cycle(GameState-Player):-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).

choose_move(_-Player,RowIndex-PositionIndex):-
    Player \= 'PC',
    Player \= 'PC1',
    Player \= 'PC2',
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

choose_move(GameState-_, RowIndex-PositionIndex):-
    choose_pc_level(Level),
    % valid_moves(GameState-Player, Moves),
    choose_move(GameState, Level, RowIndex-PositionIndex).


choose_move(GameState, 1, RowIndex-PositionIndex):-
    length(GameState,Length),
    random(0,Length,RowIndex), 
    random(0,Length,PositionIndex).

choose_move(GameState, _, RowIndex-PositionIndex):-
    % setof(Value-Mv, NewState^( member(Mv, Moves),
    % move(GameState, Mv, NewState),
    % evaluate_board(NewState, Value) ), [_V-Move|_]).
    choose_move(GameState, 1, RowIndex-PositionIndex).
    


valid_moves(GameState-Player, Moves):-
    findall(RowIndex-PositionIndex, move(GameState-Player, RowIndex-PositionIndex, _), Moves).


valid_move(GameState-Player,RowIndex-PositionIndex):-
    valid_bounds(GameState-Player,RowIndex-PositionIndex),
    valid_empty_positon(GameState-Player,RowIndex-PositionIndex).
    % TODO: Validate if the position is adjacent (othogonally or diagonally) to the last position your opponent played 

valid_bounds(GameState-_,RowIndex-PositionIndex):-
    length(GameState,Length),
    RowIndex >= 0,
    PositionIndex >= 0,
    RowIndex < Length,
    PositionIndex < Length.

valid_bounds(GameState-Player,_):-
    write('\n\n\n'),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----   Option out of bounds  -----\n'),
    write('-----       pls try again     -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    game_cycle(GameState-Player).

valid_empty_positon(GameState-_,RowIndex-PositionIndex):-
    getRow(RowIndex, GameState, Row),
    getPosition(PositionIndex, Row, Position),
    Position == ' '.

valid_empty_positon(GameState-Player,_):-
    write('\n\n\n'),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('----- The position you chose  -----\n'),
    write('-----       is not empty      -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    game_cycle(GameState-Player).

next_player(1, 2).
next_player(2, 1).
next_player('PC', 'Human').
next_player('Human', 'PC').
next_player('PC1', 'PC2').
next_player('PC2', 'PC1').

move(GameState-Player, RowIndex-PositionIndex, NewGameState):-
    valid_move(GameState-Player,RowIndex-PositionIndex),
    replace_row(GameState-Player, RowIndex-PositionIndex, NewGameState).

choose_pc_level(Level):-
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----      Choose the Level   -----\n'),
    write('-----      of the PC player   -----\n'),
    write('-----                         -----\n'),
    write('-----          Level1:        -----\n'),
    write('-----        random move      -----\n'),
    write('-----                         -----\n'),
    write('-----          Level2:        -----\n'),
    write('-----         best move       -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    read(Level).



% value(+GameState, +Player, -Value) (Aqui ou no GameState.pl?)
% choose_move(+GameState, +Level, -Move).

% GameMode
%   pvc / cvp
%   pvp
%   cvc

% Difficulty Level
%   Level1: random valid move
%   Level2: best move (greedy/blind algorithm)