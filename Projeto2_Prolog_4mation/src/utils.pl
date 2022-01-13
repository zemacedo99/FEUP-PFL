getRow(0, [Row|_], Row).

getRow(RowIndex, [_|T], Row):-
    getRow(RowIndexTemp, T, Row),
    RowIndex is RowIndexTemp + 1.

getElement(0, [Element|_], Element).

getElement(ElementIndex, [_|T], Element):-
    getElement(ElementIndexTemp, T, Element),
    ElementIndex is ElementIndexTemp + 1.