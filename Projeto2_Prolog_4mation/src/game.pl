% display_game(+GameState)
% initial_state(+Size, -GameState)
% move(+GameState, +Move, -NewGameState)
% game_over(+GameState, -Winner)
% valid_moves(+GameState, -ListOfMoves) (Aqui ou no board.pl?)
% value(+GameState, +Player, -Value) (Aqui ou no board.pl?)
% choose_move(+GameState, +Level, -Move).

% GameMode
%   pvc / cvp
%   pvp
%   cvc

% Difficulty Level
%   Level1: random valid move
%   Level2: best move (greedy/blind algorithm)