

game_over(Game, Board, Winner) :-
	getGameBoard(Game, Board),
	
% confirm that both players have at least one soldier, if not the game is over else check if the game is over due to a stalemate
	getGameNumWhitePieces(Game, NumWhitePieces),
	getGameNumBlackPieces(Game, NumBlackPieces),
	(
		(NumWhitePieces =:= 0, NumBlackPieces > 0) ->
			(write('# Game over. Black player won, congratulations!'),
			Winner is blackPlayer, nl);
		(NumWhitePieces > 0, NumBlackPieces =:= 0) ->
			(write('# Game over. White player won, congratulations!'),
			Winner is whitePlayer, nl);
		(NumWhitePieces =:= 0, NumBlackPieces =:= 0) ->
			(write('# ERROR: unexpected game over.'), nl); 
		(NumWhitePieces > 0, NumBlackPieces > 0) ->
			
			FrontCellRow is 1,
			FrontCellColA is whitePlayer(DestTowCol) + 1,
			FrontCellColB is whitePlayer(DestTowCol) + -1,
			
			FrontCellRow2 is 8,
			FrontCellCol2A is blackPlayer(DestTowCol2) + 1,
			FrontCellCol2B is blackPlayer(DestTowCol2) + -1,
		
						
			% check if the white tower have a back soldier near
			getMatrixElemAt(FrontCellRow, FrontCellColA, Board, CellA),
			getMatrixElemAt(FrontCellRow, DestTowCol, Board, CellB),
			getMatrixElemAt(FrontCellRow, FrontCellColB, Board, CellC),
			(
			(CellA == blackCell)->
				(write('# Game over. Black player won, congratulations!'),
				Winner is blackPlayer, nl);
			(CellB == blackCell)->
				(write('# Game over. Black player won, congratulations!'),
				Winner is blackPlayer, nl);
			(CellC == blackCell)->
				(write('# Game over. Black player won, congratulations!'),
				Winner is blackPlayer, nl)
			),
			
			% check if the black tower have a white soldier near
			getMatrixElemAt(FrontCellRow2, FrontCellCol2A, Board, Cell2A),
			getMatrixElemAt(FrontCellRow2, DestTowCol2, Board, Cell2B),
			getMatrixElemAt(FrontCellRow2, FrontCellCol2B, Board, Cell2C),
			(
			(Cell2A == whiteCell)->
				(write('# Game over. White player won, congratulations!'),
				Winner is whitePlayer, nl);
			(Cell2B == whiteCell)->
				(write('# Game over. White player won, congratulations!'),
				Winner is whitePlayer, nl);
			(Cell2C == whiteCell)->
				(write('# Game over. White player won, congratulations!'),
				Winner is whitePlayer, nl)
			)
			
	),
	nl,
	pressEnterToContinue, !. 
		