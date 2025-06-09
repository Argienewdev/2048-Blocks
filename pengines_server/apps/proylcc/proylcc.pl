:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5	
	]).

/*
Predicados auxiliares:
	-Reemplazar un elemento en cierto indice por otro, produciendo una nueva lista
*/

% conditional_replace_at_index(+Index, +List, +OldElement, +NewElement, -ResultList)
% Succeeds only if OldElement is at Index; replaces it with NewElement (One based).

conditional_replace_at_index(1, [OldElement|Tail], OldElement, NewElement, [NewElement|Tail]).
conditional_replace_at_index(Index, [Head|Tail], OldElement, NewElement, [Head|ResultTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    conditional_replace_at_index(NewIndex, Tail, OldElement, NewElement, ResultTail).

/**
 * randomBlock(+Grid, -Block)
 */

%Test comment

randomBlock([
	4,2,8,64,32,
	2,-,-,4,16,
	-,-,-,-,2,
	-,-,-,-,16,
	-,-,-,-,2,
	-,-,-,-,-,
	-,-,-,-,-
], 2).

randomBlock(_Grid, 4).

/**
 * shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects) 
 * RGrid es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 * 
 *shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)

 * La consulta da un bloque, en una columna, dada una grilla, de n columnas, debe retornar una lista de grillas
 * que son el paso a paso de como va cambiando la grilla. Luego pide la ultima de esas grillas y en base
 * a esa ultima pide un bloque random para seguir jugando.
 */

shoot(Block, Lane, Grid, Col, [effect(FallGrid, [])]) :-
	block_fall(Block, Lane, Grid, 0, Col, FallGrid).
	

%Caso base (final): Se verifica la ultima fila y se inserta el elemento
block_fall(Block, Lane, Grid, DRows, Col, FallGrid) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRows =:= Rows,
	Index is ((Col * DRows) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, FallGrid), !.

%Caso LANE ocupada completamente
block_fall(_Block, _Lane, Grid, DRows, Col, Grid) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRows =:= Rows,
	!.

%Caso recursivo: Se verifica la primera fila y se inserta el elemento si se puede
block_fall(Block, Lane, Grid, Row, Col, FallGrid) :-
	Index is ((Col * Row) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, FallGrid), !.

%Sino, se verifica la proxima fila hasta llegar al limite (caso base)
block_fall(Block, Lane, Grid, Row, Col, FallGrid) :-
	IRow is (Row + 1),
	block_fall(Block, Lane, Grid, IRow, Col, FallGrid).

