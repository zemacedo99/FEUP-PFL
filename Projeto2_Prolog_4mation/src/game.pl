:- consult('utils.pl').

% display_game(+GameState)
% initial_state(+Size, -GameState)
% move(+GameState, +Move, -NewGameState)

% move(GameState, Move, NewGameState):-
%     write(Move).

% game_over(+GameState, -Winner)
% valid_moves(+GameState, -ListOfMoves) (Aqui ou no GameState.pl?)

game_cycle(GameState):-
    display_game(GameState),
    choose_move(GameState,Move).  

choose_move(GameState,RowIndex-PositionIndex):-
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
    read(PositionIndex),
    valid_move(GameState,RowIndex-PositionIndex).


valid_move(GameState,RowIndex-PositionIndex):-
    valid_bounds(GameState,RowIndex-PositionIndex),
    valid_empty_positon(GameState,RowIndex-PositionIndex).

valid_bounds(GameState,RowIndex-PositionIndex):-
    length(GameState,Length),
    RowIndex >= 0,
    PositionIndex >= 0,
    RowIndex < Length,
    PositionIndex < Length.

valid_bounds(GameState,_):-
    write('\n\n\n'),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('-----   Option out of bounds  -----\n'),
    write('-----       pls try again     -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    game_cycle(GameState).

valid_empty_positon(GameState,RowIndex-PositionIndex):-
    getRow(RowIndex, GameState, Row),
    getPosition(PositionIndex, Row, Position),
    Position == ' '.

valid_empty_positon(GameState,_):-
    write('\n\n\n'),
    write('-----------------------------------\n'),
    write('-----                         -----\n'),
    write('----- The position you chose  -----\n'),
    write('-----       is not empty      -----\n'),
    write('-----                         -----\n'),
    write('-----------------------------------\n'),
    game_cycle(GameState).



% value(+GameState, +Player, -Value) (Aqui ou no GameState.pl?)
% choose_move(+GameState, +Level, -Move).

% GameMode
%   pvc / cvp
%   pvp
%   cvc

% Difficulty Level
%   Level1: random valid move
%   Level2: best move (greedy/blind algorithm)