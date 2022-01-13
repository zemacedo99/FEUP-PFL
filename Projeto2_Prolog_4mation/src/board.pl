:- use_module(library(between)).
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

initial_state(0,GameState).

initial_state(Size,GameState):-
    NewSize is Size - 1,
    make_row(NewSize,['_'],Row),
    make_board(NewSize,Row,[Row],GameState).

make_board(0,Row,GameState,GameState).

make_board(Size,Row,CurrentBoard,GameState):-
    append(CurrentBoard,[Row], NewBoard),
    NewSize is Size - 1,
    make_board(NewSize,Row,NewBoard,GameState).

make_row(0,Row,Row).

make_row(Size,CurrentRow,Row):-
    append(CurrentRow,['_'], NewRow),
    NewSize is Size - 1,
    make_row(NewSize,NewRow,Row).



%get_board
%display_game(+GameState)

display_game(Board):-
    write('\n\n\n'),
    length(Board,Length),
    print_line(Length),
    print_rows(Board,0),
    print_column(Length),
    write('\n\n\n').

print_column(Length):-
    RealLength is Length - 1,
    findall(Element, between(0, RealLength, Element), List),
    print_column_element(List).

print_column_element([]):- 
    write('\n').

print_column_element([Element | OtherElements]):-
    write(' '),
    write(Element),
    print_column_element(OtherElements).

print_line(0):-
    write('\n').

print_line(Length):-
    write(' _'),
    NewLenght is Length - 1,
    print_line(NewLenght).

print_rows([], _ ):- !.

print_rows([Row | OtherRows], RowIndex):-
    print_pieces(Row, 0),
    write('|'),
    write(RowIndex),
    write('\n'),
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