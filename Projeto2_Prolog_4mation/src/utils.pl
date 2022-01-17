getRow(0, [Row|_], Row).

getRow(RowIndex, [_|T], Row):-
    getRow(RowIndexTemp, T, Row),
    RowIndex is RowIndexTemp + 1.

getPosition(0, [Position|_], Position).

getPosition(PositionIndex, [_|T], Position):-
    getPosition(PositionIndexTemp, T, Position),
    PositionIndex is PositionIndexTemp + 1.



replace_row([Row | Rest ]-Player, 0-PositionIndex, [NewRow | Rest ]):-
    Player \= 2,
    Player \= 'PC',
    Player \= 'PC2',
    replace_positon(PositionIndex, 'X', Row, NewRow).

replace_row([Row | Rest ]-_, 0-PositionIndex, [NewRow | Rest ]):-
    replace_positon(PositionIndex, 'O', Row, NewRow).

replace_row([ Row  | Rest]-Player, RowIndex-PositionIndex, [ Row | NewRest]):-
    Next is RowIndex - 1,
    replace_row(Rest-Player, Next-PositionIndex,NewRest).

replace_positon(0, Player, [_ | Rest] , [Player | Rest ]).

replace_positon(PositionIndex, Player, [ Temp | Rest], [ Temp | NewRest]):-
    Next is PositionIndex - 1,
    replace_positon(Next, Player , Rest, NewRest).