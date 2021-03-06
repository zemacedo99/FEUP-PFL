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

%!      initial_state(+Size, -GameState) is det.

initial_state(0,_).

initial_state(Size,Board):-
    Piece = ' ',
    NewSize is Size - 1,
    make_row(NewSize,Piece,[Piece],Row),
    make_board(NewSize,Row,[Row],Board).

%!      make_board(+Size,+Row,+[Row],-Board) is multi.
%
%       True when Size is 0.
make_board(0,_,Board,Board).

make_board(Size,Row,CurrentBoard,Board):-
    append(CurrentBoard,[Row], NewBoard),
    NewSize is Size - 1,
    make_board(NewSize,Row,NewBoard,Board).

%!      make_row(+Size,+Piece,+[Piece],-Row) is multi.
%
%       True when Size is 0.
make_row(0,_,Row,Row).

make_row(Size,Piece,CurrentRow,Row):-
    append(CurrentRow,[Piece], NewRow),
    NewSize is Size - 1,
    make_row(NewSize,Piece,NewRow,Row).


%!      display_game(+Board) is det.
display_game(Board):-
    write('\n\n\n'),
    length(Board,Length),
    print_line(Length),
    print_rows(Board,0),
    print_column_number(Length),
    write('\n\n\n'),!.

%!      display_game(+GameState) is det.
display_game(Board-Player):-
    write('\n'),
    write(' Player '),
    write(Player),
    write(' Turn:'),
    write('\n'),
    write('\n'),
    length(Board,Length),
    print_line(Length),
    print_rows(Board,0),
    print_column_number(Length),
    print_player_piece(Player),
    write('\n\n\n'),!.

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

print_player_piece(Player):-
    Player \= 2,
    Player \= 'PC',
    Player \= 'PC2',
    write('\n'),
    write('\n'),
    write(' Player '),
    write(Player),
    write(' piece: '),
    write('X'),   
    write('\n').

print_player_piece(Player):-
    write('\n'),
    write('\n'),
    write(' Player '),
    write(Player),
    write(' piece: '),
    write('O'),  
    write('\n').

