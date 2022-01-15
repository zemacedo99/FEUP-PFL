:- consult('utils.pl').

% display_game(+GameState)
% initial_state(+Size, -GameState)
% move(+GameState, +Move, -NewGameState)

% move(GameState, Move, NewGameState):-
%     write(Move).

% game_over(+GameState, -Winner)
% valid_moves(+GameState, -ListOfMoves) (Aqui ou no GameState.pl?)

game_cycle(GameState-Player):-
    display_game(GameState-Player),
    choose_move(GameState-Player,Move).  

% game_cycle(GameState-Player):-
%     game_over(GameState, Winner), !,
%     congratulate(Winner).

% game_cycle(GameState-Player):-
%     choose_move(GameState, Player, Move),
%     move(GameState, Move, NewGameState),
%     next_player(Player, NextPlayer),
%     display_game(GameState-NextPlayer), !,
%     game_cycle(NewGameState-NextPlayer).

choose_move(GameState-Player,RowIndex-PositionIndex):-
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
    valid_move(GameState-Player,RowIndex-PositionIndex).


valid_move(GameState-Player,RowIndex-PositionIndex):-
    valid_bounds(GameState-Player,RowIndex-PositionIndex),
    valid_empty_positon(GameState-Player,RowIndex-PositionIndex).

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



% value(+GameState, +Player, -Value) (Aqui ou no GameState.pl?)
% choose_move(+GameState, +Level, -Move).

% GameMode
%   pvc / cvp
%   pvp
%   cvc

% Difficulty Level
%   Level1: random valid move
%   Level2: best move (greedy/blind algorithm)