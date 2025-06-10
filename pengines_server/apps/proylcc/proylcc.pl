:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5	
	]).
:- use_module(library(lists)).
/*
Predicados auxiliares:
	-Reemplazar un elemento en cierto indice por otro, produciendo una nueva lista
*/

% conditional_replace_at_index(+Index, +List, +OldElement, +NewElement, -ResultList)
% Succeeds only if OldElement is at Index; replaces it with NewElement (One based).

%retorna el valor mas grande de la grilla y tambien el menor, con un predicado de prolog de la libreria lists
%excluyo todos los guiones de la grilla y retorno una lista de numeros.
min_max_grid_values(Grid, Min, Max):- exclude(es_guion,Grid, Numeros), min_list(Numeros, Min), max_list(Numeros, Max).

es_guion(X) :- X == '-'.

es_potencia_de_dos(1).  % 2^0 = 1 es potencia de dos
es_potencia_de_dos(N) :-
    integer(N),
    N > 1,
    0 is N mod 2,
    N2 is N // 2,
    es_potencia_de_dos(N2).

conditional_replace_at_index(1, [OldElement|Tail], OldElement, NewElement, [NewElement|Tail]).
conditional_replace_at_index(Index, [Head|Tail], OldElement, NewElement, [Head|ResultTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    conditional_replace_at_index(NewIndex, Tail, OldElement, NewElement, ResultTail).

/**
 * randomBlock(+Grid, -Block)


randomBlock([
	4,2,8,64,32,
	2,-,-,4,16,
	-,-,-,-,2,
	-,-,-,-,16,
	-,-,-,-,2,
	-,-,-,-,-,
	-,-,-,-,-
], Block):- min_max_grid_values([
	4,2,8,64,32,
	2,-,-,4,16,
	-,-,-,-,2,
	-,-,-,-,16,
	-,-,-,-,2,
	-,-,-,-,-,
	-,-,-,-,-
], Min, Max), random_between(Min, Max, Block).
 */
randomBlock(Grid, Block) :- min_max_grid_values(Grid, Min, Max), findall(X, (between(Min, Max, X), es_potencia_de_dos(X)), Potencias), random_member(Block, Potencias).
%encuentra el mayor y el menor, recorre todos los valores entre el minimo y el maximo, si se cumple que es potencia de dos, lo agrega a la lista de Potencias,
% luego asigna a Block un valor random de la lista de potencias

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
	block_insert(Block, Lane, Grid, 0, Col, FallGrid).

%Caso base (final): Se verifica la ultima fila y se inserta el elemento
block_insert(Block, Lane, Grid, DRows, Col, FallGrid) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRows =:= Rows,
	Index is ((Col * DRows) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, FallGrid), !.

%Caso LANE ocupada completamente
block_insert(_Block, _Lane, Grid, DRows, Col, Grid) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRows =:= Rows,
	!.

%Caso recursivo: Se verifica la primera fila y se inserta el elemento si se puede
block_insert(Block, Lane, Grid, Row, Col, FallGrid) :-
	Index is ((Col * Row) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, FallGrid), !.

%Sino, se verifica la proxima fila hasta llegar al limite (caso base)
block_insert(Block, Lane, Grid, Row, Col, FallGrid) :-
	IRow is (Row + 1),
	block_insert(Block, Lane, Grid, IRow, Col, FallGrid).