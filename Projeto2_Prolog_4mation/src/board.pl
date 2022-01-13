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
    print_column_number(Length),
    write('\n\n\n').

print_column_number(Length):-
    RealLength is Length - 1,
    findall(Element, between(0, RealLength, Element), List),
    print_column_element(List).

print_column_element([]):- 
    write('\n').

print_column_element([Element | OtherElements]):-
    write('  '),
    write(Element),
    write(' '),
    print_column_element(OtherElements).

print_line(0):-
    write('\n').

print_line(Length):-
    write(' ___'),
    NewLenght is Length - 1,
    print_line(NewLenght).

print_rows([], _ ):- 
    write('\n').

print_rows([Row | OtherRows], RowIndex):-
    print_slash_row(Row),
    print_pieces(Row, 0),
    write(RowIndex),
    write('\n'),
    print_empty_row(Row),
    NextRowIndex is RowIndex + 1,
    print_rows(OtherRows, NextRowIndex).

print_pieces([], _ ):-
    write('|'). 

print_pieces([Piece | RestRow], PieceIndex):-
    print_piece(Piece),
    NextPieceIndex is PieceIndex + 1,
    print_pieces(RestRow, NextPieceIndex).

print_piece(Piece):-
    write('|'),
    write(' '),
    write('X'),  % mudar para piece
    write(' ').

print_empty_row([]):-
    write('|\n').

print_empty_row([EmptyPiece | RestRow]):-
    print_empty_piece,
    print_empty_row(RestRow).

print_empty_piece:-
    write('|'),
    write('_'),
    write('_'),
    write('_').

print_slash_row([]):-
    write('|\n').

print_slash_row([SlashPiece | RestRow]):-
    print_slash_piece,
    print_slash_row(RestRow).

print_slash_piece:-
    write('|'),
    write(' '),
    write(' '),
    write(' ').
    
    
    

%get_piece
%place_piece
%find_adjacent