%initial_board

% initial_board([
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_'],
%     ['_','_','_','_','_','_','_']
% ]).

%initial_state(+Size, -GameState)

initial_state(Size,GameState):-
    NewSize is Size - 1,
    make_row(NewSize,['_'],Row),
    make_board(NewSize,Row,[Row],GameState).

make_board(0,Row,CurrentBoard,GameState):- 
    GameState = CurrentBoard, !.

make_board(Size,Row,CurrentBoard,GameState):-
    append(CurrentBoard,[Row], NewBoard),
    NewSize is Size - 1,
    make_board(NewSize,Row,NewBoard,GameState).

make_row(0,CurrentRow,Row):- Row = CurrentRow, !.

make_row(Size,CurrentRow,Row):-
    append(CurrentRow,['_'], NewRow),
    NewSize is Size - 1,
    make_row(NewSize,NewRow,Row).



%get_board
%display_game(+GameState)

display_game(Board):-
    write('\n\n\n'),
    write(' _____________\n'),
    print_rows(Board,0),
    write('\n\n\n').

print_rows([], _ ):- !.

print_rows([Row | OtherRows], RowIndex):-
    print_pieces(Row, RowIndex),
    write('|\n'),
    NextRowIndex is RowIndex + 1,
    print_rows(OtherRows, NextRowIndex).

print_pieces([], _ ):- !. 

print_pieces([Piece | RestRow], PieceIndex):-
    print_piece(Piece),
    NextPieceIndex is PieceIndex + 1,
    print_pieces(RestRow, NextPieceIndex).

print_piece(Piece):-
    write('|'),
    write(Piece).


%get_piece
%place_piece
%find_adjacent