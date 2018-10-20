
display_game(Board,Player):-
	initialBoard(Board),
	getPlayer(Player),
	printHeaderRow(),
	printBoard(Board).
	
getPlayer(redPlayer).
getPlayer(blackPlayer).

getPlayerName(redPlayer, 'Red').
getPlayerName(blackPlayer, 'Black').

getPiece(redPiece).
getPiece(blackPiece).


/*Atribui um simbolo as celulas e torres dos jogadores*/
getAssignedSymbol(emptyCell, ' ').
getAssignedSymbol(redCell, 'O').
getAssignedSymbol(blackCell, '#').
getAssignedSymbol(redTower, '*').
getAssignedSymbol(blackTower, 'x').
getAssignedSymbol(_, '?').

printHeaderRow:-
	write('        A     B     C     D     E     F     G     H     I     J    '),nl,
	write('      ___________________________________________________________').
	
rowIdentifiersList(['  1  ', '  2  ', '  3  ', '  4  ', '  5  ', '  6  ', '  7  ', '  8  ','  9  ','  10 ']).

printBoard([Line | Tail], Player):-
	rowIdentifiersList(RowIdentifiers),
	printBoardRows([Line | Tail], RowIdentifiers), nl, !.

printBoardRows([], []).
printBoardRows([Line | Tail], [RowIdentifiersListHead | RowIdentifiersListTail]):-
	printSingleBoardRow(Line, RowIdentifiersListHead),
	printBoardRows(Tail, RowIdentifiersListTail).

printSingleRow([], []).
printSingleRow(Line, RowIdentifiersListHead):-
	length(Line, Length),
	createSeparatorN(Length, '_____|', Separator),
	createSeparatorN(Length, '     |', Separator2),
	write('     '), write('|'), printList(Separator2), nl,
	write(RowIdentifiersListHead), write('|'),
	printBoardRowValues(Line), nl,
	write('     '), write('|'), printList(Separator), nl.

createSeparatorN(0, _, []).
createSeparatorN(N, SS, [SS | Ls]):-
	N1 is N-1,
	createSeparatorN(N1, SS, Ls).

printBoardRowValues([]).
printBoardRowValues([Head | Tail]):-
	getAssignedSymbol(Head, Piece),
	write('  '), 
	write(Piece), 
	write('  |'),
	printBoardRowValues(Tail).

	
/* Esquema do tabuleiro */

initialBoard([
	[emptyCell, redTower, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
	[emptyCell, redCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell],
	[emptyCell, redCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell],
	[emptyCell, emptyCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell, emptyCell, redCell],
	[emptyCell, redCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
	[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, blackCell, emptyCell],
	[blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, emptyCell, emptyCell],
	[blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell],
	[blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell, blackCell, emptyCell],
	[emptyCell, blackTower, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]]).
		
	