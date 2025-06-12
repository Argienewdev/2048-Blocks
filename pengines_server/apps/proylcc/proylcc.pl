:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5	
	]).
:- use_module(library(lists)).
/*
DUDAS:
	-En el caso de que una LANE este completamente ocupada, debemos contemplar
	que el usuario dispare de nuevo? El juego lo permite si apuntas a la ranura 
	entre los bloques
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


es_guion('-').

%retorna el maximo actual de la grilla
max_actual(Grid, Max):- 
	exclude(es_guion, Grid, Numeros), 
	max_list(Numeros, Max).

/*
min_max_grid_values_permited retorna el rengo menor y mayor segun grilla, luego segun formula.
min_max_grid_values_permited(MaxActual, MinPermitido, MaxPermitido)
*/
min_max_grid_values_permited(Max, 2, 4) :- member(Max, [2,4,8]), !.
min_max_grid_values_permited(16, 2, 8) :- !.
min_max_grid_values_permited(32, 2, 16) :- !.
min_max_grid_values_permited(64, 2, 32) :- !.
min_max_grid_values_permited(Max, 2, 64) :- member(Max, [128,256,512]), !.
min_max_grid_values_permited(1024, 4, 128) :- !.
min_max_grid_values_permited(2048, 8, 256) :- !.
min_max_grid_values_permited(Max, 16, 512) :- member(Max, [4096,8192]), !.
min_max_grid_values_permited(16384, 32, 1024) :- !.

/* A partir de MaxAct >= 16000, el rango permitido (MinPerm y MaxPerm) se duplica
cada vez que MaxAct se duplica. Se usa piso de log(MaxAct / 16000) / log2 para contar cuántas
duplicaciones hubo respecto del valor base (16000), y se incrementan los K.
*/
min_max_grid_values_permited(MaxAct, MinPerm, MaxPerm) :-
    Base is 16000,
    K is floor(log(MaxAct / Base) / log(2)),
    ExpMin is 5 + K,
    ExpMax is 10 + K,
    MinPerm is 2 ** ExpMin,
    MaxPerm is 2 ** ExpMax.

%Retorna true si se trata de una potencia de 2
es_potencia_de_dos(1). 

es_potencia_de_dos(N) :-
    integer(N),
    N > 1,
    0 is N mod 2,
    N2 is N // 2,
    es_potencia_de_dos(N2).

/*
randomBlock se encarga de, dada una grilla, retornar un numero aleatorio valido dadas las reglas del juego.
logica: elige el maximo valor actual de la grilla, y luego segun este valor calcula el rango minimo y maximo 
basandose en grilla o formula si el MaxAct>=16000.
por ultimo crea una lista de potencias de dos que esten dentro del rango y elige uno aleatorio.
*/

randomBlock(Grid, Block) :- 
	max_actual(Grid, MaxAct), 
	min_max_grid_values_permited(MaxAct, Min, Max),
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
	block_insert(Block, Lane, Grid, 0, Col, InsertGrid, InsertIndex),
	block_fall(InsertGrid, Col, GravityGrid),
	append([effect(InsertGrid, [])], [effect(GravityGrid, [])], Effects).

/*
Block insert se encarga de insertar el bloque recien disparado en la posicion correspondiente y
retornar el indice (en base 0) donde se insertó (-1 es no insertado).
Caso base (final): Se verifica la ultima fila y se inserta el elemento
Caso LANE ocupada completamente: Retorna la misma grilla sin cambios (Problema: Random block actúa)
Caso recursivo 1: Se verifica la primera fila y se inserta el elemento si se puede
Caso recursivo 2: Se verifica la proxima fila hasta llegar al limite (caso base)
*/

block_insert(Block, Lane, Grid, DRow, Col, InsertGrid, InsertIndex) :-
	%DRow: "Diminished Row" o "Fila disminuida" hace referencia a la anteultima fila
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	Index is ((Col * DRow) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid),
	InsertIndex is Index - 1, !.

block_insert(_Block, _Lane, Grid, DRow, Col, Grid, -1) :-
	length(Grid, GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	!.

block_insert(Block, Lane, Grid, Row, Col, InsertGrid, InsertIndex) :-
	Index is ((Col * Row) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid),
	InsertIndex is Index - 1, !.

block_insert(Block, Lane, Grid, Row, Col, InsertGrid, InsertIndex) :-
	NextRow is (Row + 1),
	block_insert(Block, Lane, Grid, NextRow, Col, InsertGrid, InsertIndex).

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

/*
Logica para fusiones:
	Nuevo valor: 
		Se basa en la cantidad de bloques con los que se fusiona.
		El resultado sera el bloque duplicado tantas veces como bloques
		con los que se fusiono.
		Ej1.
			se dispara un 2 contra 3 bloques numero 2. El numero 2 se duplica 3 veces por lo que resulta 16
		Ej2.
			se dispara un 4 contra 2 bloques numero 4. El numero 4 se duplica 2 veces por lo que resulta 16
		Formula: bloque x 2^[cantidad de bloques con los que se fusiona]
	Posicion final de fusion:
		Para cualquier etapa:
			-Si solo puede fusionarse con un bloque y es el de arriba entonces la fusion culmina en la posicion de arriba.
			-Si hay posibilidad de fusionar mas de 2 bloques, se hace y culmina en el centro de la fusion.
			-En cualquier otro caso, la fusion culmina en la posicion del bloque que se movio ultimo.
*/