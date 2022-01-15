:- use_module(library(between)).
%initial_board

% initial_board([
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
%     [' ',' ',' ',' ',' ',' ',' '],
% ]).

%initial_state(+Size, -GameState)

initial_state(0,_).

initial_state(Size,GameState-Player):-
    Player = 1,
    Piece = ' ',
    NewSize is Size - 1,
    make_row(NewSize,Piece,[Piece],Row),
    make_board(NewSize,Row,[Row],GameState).

make_board(0,_,GameState,GameState).

make_board(Size,Row,CurrentBoard,GameState):-
    append(CurrentBoard,[Row], NewBoard),
    NewSize is Size - 1,
    make_board(NewSize,Row,NewBoard,GameState).

make_row(0,_,Row,Row).

make_row(Size,Piece,CurrentRow,Row):-
    append(CurrentRow,[Piece], NewRow),
    NewSize is Size - 1,
    make_row(NewSize,Piece,NewRow,Row).



%get_board
%display_game(+GameState)

display_game(GameState-Player):-
    write('\n\n\n'),
    length(GameState,Length),
    print_line(Length),
    print_rows(GameState,0),
    print_column_number(Length),
    write('\n'),
    write(' Player '),
    write(Player),
    write(' Turn:'),
    write('\n'),
    write('\n').

print_column_number(Length):-
    RealLength is Length - 1,
    findall(Element, between(0, RealLength, Element), List),
    print_column_element(List).

print_column_element([]):- 
    write('\n').

print_column_element([Element | OtherElements]):-
    write('   '),
    write(Element),
    write('  '),
    print_column_element(OtherElements).

print_line(0):-
    write('\n').

print_line(Length):-
    write(' _____'),
    NewLenght is Length - 1,
    print_line(NewLenght).

print_rows([], _ ):- 
    write('\n').

print_rows([Row | OtherRows], RowIndex):-
    print_row(Row,' '),
    print_pieces(Row, 0),
    print_row_index(RowIndex),
    print_row(Row,'_'),
    NextRowIndex is RowIndex + 1,
    print_rows(OtherRows, NextRowIndex).

print_row([], _ ):-
    write('|\n'). 

print_row([ _ | RestRow], Element):-
    print_element(Element),
    print_row(RestRow,Element).

print_element(Element):-
    write('|'),
    write(Element),
    write(Element),
    write(Element),
    write(Element),  
    write(Element).

print_row_index(RowIndex):-
    write('  '),
    write(RowIndex),
    write('\n').

print_pieces([], _ ):-
    write('|'). 

print_pieces([Piece | RestRow], PieceIndex):-
    print_piece(Piece),
    NextPieceIndex is PieceIndex + 1,
    print_pieces(RestRow, NextPieceIndex).

print_piece(Piece):-
    write('|'),
    write('  '),
    write(Piece),  
    write('  ').
    
    
    

%get_piece
%place_piece
%find_adjacent