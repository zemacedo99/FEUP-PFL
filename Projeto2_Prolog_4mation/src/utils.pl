getRow(0, [Row|_], Row).

getRow(RowIndex, [_|T], Row):-
    getRow(RowIndexTemp, T, Row),
    RowIndex is RowIndexTemp + 1.

getPosition(0, [Position|_], Position).

getPosition(PositionIndex, [_|T], Position):-
    getPosition(PositionIndexTemp, T, Position),
    PositionIndex is PositionIndexTemp + 1.



replace_row([Row | _ ]-Player, 0-PositionIndex, [NewRow | _ ]):-
    Player == 1,
    replace_positon(PositionIndex, 'X', Row, NewRow).

replace_row([Row | _ ]-Player, 0-PositionIndex, [NewRow | _ ]):-
    Player == 2,
    replace_positon(PositionIndex, 'O', Row, NewRow).

replace_row([ _  | Rest]-Player, RowIndex-PositionIndex, [ _ | NewRest]):-
    Next is RowIndex - 1,
    replace_row(Rest-Player, Next-PositionIndex,NewRest).

replace_positon(0, Player, _ , [Player | _ ]).

replace_positon(PositionIndex, Player, [ _ | Rest], [ _ | NewRest]):-
    Next is PositionIndex - 1,
    replace_positon(Next, Player , Rest, NewRest).