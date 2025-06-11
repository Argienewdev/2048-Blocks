:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5	
	]).
:- use_module(library(lists)).
/*
DUDAS:
	-En el caso de que una LANE este completamente ocupada, debemos contemplar
	que el usuario dispare de nuevo? El juego lo permite si apuntas a la ranura entre los bloques
*/

%Predicados auxiliares:

/*
conditional_replace_at_index(+Index, +List, +OldElement, +NewElement, -ResultList)
Retorna true solo si existe OldElement en Index. Lo reemplaza por NewElement. Indice basado en 1
*/

conditional_replace_at_index(1, [OldElement|Tail], OldElement, NewElement, [NewElement|Tail]).

conditional_replace_at_index(Index, [Head|Tail], OldElement, NewElement, [Head|ResultTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    conditional_replace_at_index(NewIndex, Tail, OldElement, NewElement, ResultTail).

/*
replace_at_index(+List, +Index, +Element, -ResultList)
Reemplaza un elemento por otro dado un indice en base 1.
*/

replace_at_index([_ | T], 0, Elem, [Elem | T]) :- !.

replace_at_index([H | T], Index, Elem, [H | R]) :-
    NextIndex is Index - 1,
    replace_at_index(T, NextIndex, Elem, R).

/*
min_max_grid_values retorna el numero mas grande de la lista asi como tambien el mas chico.
Se excluyen todos los guiones para luego buscar el numero mas pequeño y el mas grande.
*/
es_guion('-').

min_max_grid_values(Grid, Min, Max):- 
	exclude(es_guion, Grid, Numeros), 
	min_list(Numeros, Min), 
	max_list(Numeros, Max).



%Retorna true si se trata de una potencia de 2
es_potencia_de_dos(1).  % 2^0 = 1 es potencia de dos

es_potencia_de_dos(N) :-
    integer(N),
    N > 1,
    0 is N mod 2,
    N2 is N // 2,
    es_potencia_de_dos(N2).

/*
randomBlock se encarga de, dada una grilla, retornar un numero aleatorio valido dadas las reglas del juego
En realidad:
El predicado elige un numero aleatorio potencia de dos entre el numero mas chico y mas grande de la grilla
TODO: Seguir las reglas de generacion dadas en github

Máximo de la Grilla		|	Rango			|	Observación
2, 4, 8					|	2 a 4			|	
16						|	2 a 8			|	
32						|	2 a 16			|	
64						|	2 a 32			|	
128, 256, 512			|	2 a 64			|	
1024					|	4 a 128			|	Se retira el 2
2048					|	8 a 256			|	Se retira el 4
4096, 8192				|	16 a 512		|	Se retira el 8
16k						|	32 a 1024		|	Se retira el 16
...						|	...				|	...

*/

randomBlock(Grid, Block) :- 
	min_max_grid_values(Grid, Min, Max), 
	findall(X, (between(Min, Max, X), es_potencia_de_dos(X)), Potencias), 
	random_member(Block, Potencias).

/**
 * shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects) 
 * RGrid es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 * 
 *shoot(${shootBlock}, 
 		${lane}, 
		${gridS}, 
		${numOfColumns},  
		Effects), 
		last(Effects, effect(RGrid,_)), 
		
		randomBlock(RGrid, Block)

 * La consulta da un bloque, en una columna, dada una grilla, de n columnas, debe retornar una lista de grillas
 * que son el paso a paso de como va cambiando la grilla. Luego pide la ultima de esas grillas y en base
 * a esa ultima pide un bloque random para seguir jugando.
 */

shoot(Block, Lane, Grid, Col, Effects) :-
	block_insert(Block, Lane, Grid, 0, Col, InsertGrid),
	block_fall(InsertGrid, Col, GravityGrid),
	append([effect(InsertGrid, [])], [effect(GravityGrid, [])], Effects).

/*
Block insert se encarga de insertar el bloque recien disparado en la posicion correspondiente
Caso base (final): Se verifica la ultima fila y se inserta el elemento
Caso LANE ocupada completamente: Retorna la misma grilla sin cambios (Problema: Random block actúa)
Caso recursivo 1: Se verifica la primera fila y se inserta el elemento si se puede
Caso recursivo 2: Se verifica la proxima fila hasta llegar al limite (caso base)
*/

block_insert(Block, Lane, Grid, DRow, Col, InsertGrid) :-
	%DRow: "Diminished Row" o "Fila disminuida" hace referencia a la anteultima fila
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	Index is ((Col * DRow) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid), !.

block_insert(_Block, _Lane, Grid, DRow, Col, [effect(Grid, [])]) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	!.

block_insert(Block, Lane, Grid, Row, Col, InsertGrid) :-
	Index is ((Col * Row) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid), !.

block_insert(Block, Lane, Grid, Row, Col, InsertGrid) :-
	NextRow is (Row + 1),
	block_insert(Block, Lane, Grid, NextRow, Col, InsertGrid).

/*
Block_fall implementa la gravedad, la cual hace que los bloques caigan luego de combinaciones
	Predicados auxiliares:
		-Process_columns
			-extract_column
				-nth0 (Libreria) Retorna un elemento dado un indice en base 0
			-sort_column
				-separate_scores
    		-reinsert_column
*/

block_fall(Grid, Col, GravityGrid) :-
	process_columns(0, Col, Grid, GravityGrid).

/*
process_columns recorre la grilla extrayendo, organizando y reinsertando columnas una a una
para que los bloques no tengan espacio vacio "debajo", retornando una nueva grilla sin bloques "flotantes".
*/

process_columns(TotalCols, TotalCols, Grid, Grid) :- !.

process_columns(Index, TotalCols, GridIn, GridOut) :-
	
    extract_column(GridIn, Index, TotalCols, 0, Column),
    sort_column(Column, SortedColumn),
    reinsert_column(GridIn, Index, TotalCols, 0, SortedColumn, NextGrid),
	
    NextIndex is Index + 1,
    process_columns(NextIndex, TotalCols, NextGrid, GridOut).

/*
sort_column toma la lista que representa una columna y la ordena
diviendola en "no guiones" y "guiones" para despues concatenarla
Ej. [1,-,2,-,3] 
	no guiones es [1,2,3]
	guiones es [-,-]
Retorna [1,2,3,-,-]
La division en guiones y no guiones la hace el predicado separate_scores
*/

sort_column(OriginalColumn, SortedColumn) :-
	separate_scores(OriginalColumn, NoScores, Scores),
	append(NoScores, Scores, SortedColumn).

separate_scores([], [], []).

separate_scores(['-'|T], NoScores, ['-'|Xs]) :-
	separate_scores(T, NoScores, Xs), !.

separate_scores([H|T], [H|Hs], Scores) :-
	H \= '-', 
	separate_scores(T, Hs, Scores).

/*
extract_column calcula que posiciones de la grilla pertenecen a la columna que se le 
pide extraer para luego agregarlas a la lista a retornar, la cual contiene todos los 
elementos de la columna especificada.
Este predicado comienza por la fila 0 y termina al llegar a la ultima fila.
*/

extract_column(Grid, ColIndex, NumCols, Row, [Elem]) :- 
	length(Grid, GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
	nth0(Index, Grid, Elem), !.

extract_column(Grid, ColIndex, NumCols, NumRows, [Elem | Rest]) :-
    Index is ColIndex + NumRows * NumCols,
    nth0(Index, Grid, Elem),
    NextNumRows is NumRows + 1,
    extract_column(Grid, ColIndex, NumCols, NextNumRows, Rest).

/*
reinsert_column, dada una columna a reinsertar, la grilla, y el indice de dicha columna (basado en 0),
reemplaza los elementos que pertenezcan a la columna en la grilla por los de la nueva columna.
Este predicado comienza por la columna 0 y termina al llegar a la ultima columna.
*/

reinsert_column(GridIn, ColIndex, NumCols, Row, [Elem], GridOut) :-
	length(GridIn, GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridOut), !.

reinsert_column(GridIn, ColIndex, NumCols, Row, [Elem | Rest], GridOut) :-
    Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridNext),
    NextRow is Row + 1,
    reinsert_column(GridNext, ColIndex, NumCols, NextRow, Rest, GridOut).