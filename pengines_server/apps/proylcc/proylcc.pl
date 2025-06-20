:- module(proylcc, 
	[  
		randomBlock/2,
		shoot/5	
	]).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).

:- dynamic gridSize/1.

/*
TESTING: PARA NO TENER QUE HACER SHOOT TODO EL TIEMPO
TODO: BORRAR ANTES DE ENTREGAR
*/
%gridSize(35).

/*
DUDAS:
	-En el caso de que una LANE este completamente ocupada, debemos contemplar
	que el usuario dispare de nuevo? El juego lo permite si apuntas a la ranura 
	entre los bloques. ASUMO QUE EL USUARIO NO VA A HACER TAL COSA

	-Se puede modificar la consulta que hace game a proylcc para implementar el powerup que 
	te permite ver el bloque proximo a disparar? SI, PERO NO ES NECESARIO

	-Que tan especificos hay que ser cuando describimos los predicados que definimos?
	RTA: SIRVE DEFINIR LOS COMENTARIOS, SOLO SI NO SON COSAS EVIDENTES.
	RTA: HAY QUE HACER INFORME

	-Hay que implementar el "game over"? OPCIONAL

	-Explica mejor el booster hint
	POR COLUMNA, CUAL SERIA EL BLOQUE MAXIMO CONSEGUIDO Y EL COMBO. GRIS ESCRITO AL PIE O AL TECHO DE LA COLUMNA
	COMBO A PARTIR DE X3? A ELEGIR Y ESPECIFICAR. EL COMBO ES CUANTAS FUSIONES SE LOGRARON.

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

/*
max_actual(+Grid, -Max)
retorna el maximo actual de la grilla
*/
max_actual(Grid, Max):- 
	exclude(es_guion, Grid, Numeros), 
	max_list(Numeros, Max).

/*
min_actual(+Grid, -Min)
retorna el minimo actual de la grilla
*/
min_actual(Grid, Min):- 
	exclude(es_guion, Grid, Numeros), 
	max_list(Numeros, Min).

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
cada vez que MaxAct se duplica. Se usa K= piso de log(MaxAct / 16000) / log2 para contar cuántas
duplicaciones hubo respecto del valor base (16000), luego se incrementan K veces los exponentes.
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

/*replace_all(ListaInicial,Valor,Reemplazo,Resultado)
dada una lista inicial reemplaza cada aparicion de Valor, por un elemento deseado Reemplazo, luego retorna una lista con los valores reemplazados
*/

%caso base, llegue al final de la grilla
replace_all([],_,_,[]).
%caso 1, el valor actual es el que quiero reemplazar, sigo rearmando la lista pero en vez de el usar valor uso reemplazo
replace_all([Value|T], Value, Replacement, [Replacement|R]):- 
	replace_all(T,Value,Replacement, R).
%caso 2, el valor actual no es el que quiero reemplazar, sigo rearmando la lista con el mismo valor,
replace_All([H|T], Value, Replacement, [H|R]):-
	 H \= Value, replace_all(T, Value, Replacement, R).

%TODO: Nadie llama a remove_min, tener en cuenta que cuando es llamado, hay
%que tomar la lista de bloques que se movieron y practicar fusion en cada uno
remove_min(Grid, Col, GravityGrid):- 
	min_actual(Grid, Min),
	replace_all(Grid, Min, -, GridRemoved),
	block_fall(GridRemoved, Col, GravityGrid, _Movements).

/*
randomBlock se encarga de, dada una grilla, retornar un numero aleatorio valido dadas las reglas del juego.
logica: elige el maximo valor actual de la grilla, y luego segun este valor calcula el rango minimo y maximo 
basandose en grilla o formula si el MaxAct>=16000.
por ultimo crea una lista de potencias de dos que esten dentro del rango y elige uno aleatorio.
*/

randomBlock(Grid, Block):-
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

shoot en principio calcula la longitud de la grilla y la deja en assert para no volver a calcularla
cuando ya esta calculada, este procede del siguiente modo:
	block insert: pone el bloque donde va luego de ser disparado, retorna efecto y a donde cayo
	fusion_admin: lleva a cabo toda la serie de fusiones correspondientes y retorna los efectos
	append: junto los efectos para retornarlos
*/

shoot(Block, Lane, Grid, Col, Effects) :-
	gridSize(_), !,
	block_insert(Block, Lane, Grid, Col, InsertGrid, InsertIndex),
	fusion_admin(InsertGrid, Col, [InsertIndex], FEffects),
	append([effect(InsertGrid, [])], FEffects, Effects).

shoot(Block, Lane, Grid, Col, Effects) :-
	length(Grid, GridSize),
	assert(gridSize(GridSize)), !,
	block_insert(Block, Lane, Grid, Col, InsertGrid, InsertIndex),
	fusion_admin(InsertGrid, Col, [InsertIndex], FEffects),
	append([effect(InsertGrid, [])], FEffects, Effects).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
Fusion admin retornara una lista de efectos una vez terminadas todas las fusiones
Fusion admin usa un auxiliar para "otorgarse" un acumulador para los efectos
*/
fusion_admin(Grid, Col, Indexes, Effects) :-
	fusion_admin_aux(Grid, Col, Indexes, [], Effects).

/*
Fusion admin aux funcionara de la siguiente manera
	fusion loop: se encarga de llevar a cabo todas las fusiones simultaneas y retorna el ultimo
	efecto junto con todos los bloques nuevos que se crearon y todos los nuevos indices de bloques
	que se movieron luego de las fusiones
	block fall: aplica gravedad, retorna la nueva grilla y los bloques que se movieron

	si block fall no era necesario, entonces solo me quedo con el efecto de las fusiones y
	hago un llamado recursivo para volver a fusion admin aux con los bloques que se movieron en las
	fusiones simultaneas

	si block fall si era necesario, agrego el efecto a la lista que voy a retornar, agrego los indices
	de bloques que se movieron a la lista de indices a revisar nuevamente y hago el llamado recursivo
*/

fusion_admin_aux(_Grid, _Col, [], Acc, Acc):- !.

fusion_admin_aux(Grid, Col, Indexes, Acc, Effects) :-
	fusion_loop(Grid, Col, Indexes, FinalFusionEffect, LastGrid, NewFusionIndexes),
	(block_fall(LastGrid, Col, GravityGrid, NewGravityIndexes) ->

	append([FinalFusionEffect], [effect(GravityGrid, [])], LoopEffect),
	append(Acc, LoopEffect, NewAcc),

	append(NewGravityIndexes, NewFusionIndexes, NewIndexes),

	fusion_admin_aux(GravityGrid, Col, NewIndexes, NewAcc, Effects);
	
	append(Acc, [FinalFusionEffect], NewAcc),

	fusion_admin_aux(LastGrid, Col, NewFusionIndexes, NewAcc, Effects)), !.

/*
fusion loop usa auxiliares para otorgar acumuladores
*/
/*
TODO: Ver si esto realmente sirve o ya no
fusion_loop(Grid, Col, Indexes, FusionEffects, LastGrid, []) :-
	fusion_loop_aux(Grid, Col, Indexes, FusionEffects, LastGrid, [], [], []), !.
*/

fusion_loop(Grid, Col, Indexes, FusionEffects, LastGrid, NewIndexes) :-
	fusion_loop_aux(Grid, Col, Indexes, FusionEffects, LastGrid, [], [], NewIndexes).

/*
fusion loop aux funciona de la siguiente manera:

*/

fusion_loop_aux(Grid, _Col, [], effect(Grid, Acc), Grid, Acc, NewIndexesAcc, NewIndexesAcc) :- !.

/*
En caso de que no haya fusion no puede dar falso, entonces necesito que en ese
caso deseche el indice y siga, pero si no hago esta implicacion, debo evaluar por falla
al predicado fusion, lo que seria una perdida de tiempo porque ya fue evaluado.
*/

fusion_loop_aux(Grid, Col, [X | Xs], FinalFusionEffect, LastGrid, Acc, NewIndexesAcc, NewIndexes) :-
	(fusion(Grid, Col, X, FGrid, NewBlocksAfterFusion, NewIndex) ->
	append(NewIndex, NewIndexesAcc, NextNewIndexesAcc),
	append([NewBlocksAfterFusion], Acc, NewAcc),
	fusion_loop_aux(FGrid, Col, Xs, FinalFusionEffect, LastGrid, NewAcc, NextNewIndexesAcc, NewIndexes);

	fusion_loop_aux(Grid, Col, Xs, FinalFusionEffect, LastGrid, Acc, NewIndexesAcc, NewIndexes)).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
block_insert(+Block, +Lane, +Grid, +Col, -InsertGrid, -InsertIndex)
es una simplificacion de block insert aux, el cual es llamado con la fila
inicializada en 0.
*/
block_insert(Block, Lane, Grid, Col, InsertGrid, InsertIndex) :-
	block_insert_aux(Block, Lane, Grid, 0, Col, InsertGrid, InsertIndex).

/*
block_insert_aux(+Block, +Lane, +Grid, +DRow, +Col, -InsertGrid, -InsertIndex)
Block insert se encarga de insertar el bloque recien disparado en la posicion correspondiente y
retornar el indice (en base 0) donde se insertó (-1 es no insertado).
Caso base (final): Se verifica la ultima fila y se inserta el elemento
Caso LANE ocupada completamente: Retorna la misma grilla sin cambios (Problema: Random block actúa)
Caso recursivo 1: Se verifica la primera fila y se inserta el elemento si se puede
Caso recursivo 2: Se verifica la proxima fila hasta llegar al limite (caso base)
*/
block_insert_aux(Block, Lane, Grid, DRow, Col, InsertGrid, InsertIndex) :-
	%DRow: "Diminished Row" o "Fila disminuida" hace referencia a la anteultima fila
	gridSize(GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	Index is ((Col * DRow) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid),
	InsertIndex is Index - 1, !.

block_insert_aux(_Block, _Lane, Grid, DRow, Col, Grid, -1) :-
	gridSize(GridLength), 
	Rows is (GridLength / Col) - 1,
	DRow =:= Rows,
	!.

block_insert_aux(Block, Lane, Grid, Row, Col, InsertGrid, InsertIndex) :-
	Index is ((Col * Row) + Lane),
	conditional_replace_at_index(Index, Grid, -, Block, InsertGrid),
	InsertIndex is Index - 1, !.

block_insert_aux(Block, Lane, Grid, Row, Col, InsertGrid, InsertIndex) :-
	NextRow is (Row + 1),
	block_insert_aux(Block, Lane, Grid, NextRow, Col, InsertGrid, InsertIndex).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
block_fall(+Grid, +Col, -GravityGrid, -Movements)
Block_fall implementa la gravedad, la cual hace que los bloques caigan luego de combinaciones
	Predicados auxiliares:
		-Process_columns
			-extract_column
				-nth0 (Libreria) Retorna un elemento dado un indice en base 0
			-sort_column
				-separate_scores
    		-reinsert_column
	TODO: Block fall se puede optimizar haciendo que solo recorra las columnas alrededor de
	las ultimas fusiones
	TODO: Siempre retorno una grilla aunque no hayan cambios, CAMBIAR
	TODO: Puedo evitarlo llamando a la gravedad luego de fusiones
*/
	
block_fall(Grid, Col, Grid, Movements) :-
	process_columns(Grid, Col, Grid, Movements), !, fail.
	
block_fall(Grid, Col, GravityGrid, Movements) :-
	process_columns(Grid, Col, GravityGrid, Movements).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
process_columns(+GridIn, +TotalCols, -GridOut, -Movements)
process_columns recorre la grilla extrayendo, organizando y reinsertando columnas una a una
para que los bloques no tengan espacio vacio "debajo", retornando una nueva grilla sin bloques "flotantes".
*/

process_columns(GridIn, TotalCols, GridOut, Movements) :-
	process_columns_aux(0, TotalCols, GridIn, GridOut, [], Movements).

process_columns_aux(TotalCols, TotalCols, Grid, Grid, Acc, Movements) :- 
	reverse(Acc, Movements), !.

process_columns_aux(Index, TotalCols, GridIn, GridOut, Acc, Movements) :-
	
    extract_column(GridIn, Index, TotalCols, 0, Column),
    sort_column(Column, SortedColumn),
	find_movements(0, Column, SortedColumn, TotalCols, Index, MovementsHead),
    reinsert_column(GridIn, Index, TotalCols, 0, SortedColumn, NextGrid),
	
    NextIndex is Index + 1,
	append(MovementsHead, Acc, NewAcc),
    process_columns_aux(NextIndex, TotalCols, NextGrid, GridOut, NewAcc, Movements).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
sort_column(+OriginalColumn, -SortedColumn)
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

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
extract_column(+Grid, +ColIndex, +NumCols, +Row, -ExtractedColumn)
extract_column calcula que posiciones de la grilla pertenecen a la columna que se le 
pide extraer para luego agregarlas a la lista a retornar, la cual contiene todos los 
elementos de la columna especificada.
Este predicado comienza por la fila 0 y termina al llegar a la ultima fila.
*/

extract_column(Grid, ColIndex, NumCols, Row, [Elem]) :- 
	gridSize(GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
	nth0(Index, Grid, Elem), !.

extract_column(Grid, ColIndex, NumCols, NumRows, [Elem | Rest]) :-
    Index is ColIndex + NumRows * NumCols,
    nth0(Index, Grid, Elem),
    NextNumRows is NumRows + 1,
    extract_column(Grid, ColIndex, NumCols, NextNumRows, Rest).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
reinsert_column(+GridIn, +ColIndex, +NumCols, +Row, +ColumnToInsert, -GridOut)
reinsert_column, dada una columna a reinsertar, la grilla, y el indice de dicha columna (basado en 0),
reemplaza los elementos que pertenezcan a la columna en la grilla por los de la nueva columna.
Este predicado comienza por la columna 0 y termina al llegar a la ultima columna.
*/

reinsert_column(GridIn, ColIndex, NumCols, Row, [Elem], GridOut) :-
	gridSize(GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridOut), !.

reinsert_column(GridIn, ColIndex, NumCols, Row, [Elem | Rest], GridOut) :-
    Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridNext),
    NextRow is Row + 1,
    reinsert_column(GridNext, ColIndex, NumCols, NextRow, Rest, GridOut).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
find_movements(+Iteration, +Column, +SortedColumn, +TotalColumns, +ColumnToCheck, -Movements)
Este predicado revisa, dada una columna sin organizar y una organizada, que bloques se movieron
y a donde. Retorna a que posicion de la grilla se movieron.
*/

find_movements(_Iteration, [], [], _TotalColumns, _ColumnToCheck, []):- !.

find_movements(Iteration, ['-' | Xs], [Y | Ys], TotalColumns, ColumnToCheck, [IndexOnGrid | Zs]) :-
	Y \= '-',
	IndexOnGrid is ColumnToCheck + (Iteration * TotalColumns),
	NextIteration is Iteration + 1,
	find_movements(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Zs), !.
	
find_movements(Iteration, [X | Xs], [Y | Ys], TotalColumns, ColumnToCheck, [IndexOnGrid | Zs]) :-
	X \= Y,
	Y \= '-',
	IndexOnGrid is ColumnToCheck + (Iteration * TotalColumns),
	NextIteration is Iteration + 1,
	find_movements(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Zs), !.

find_movements(Iteration, [_X | Xs], [_Y | Ys], TotalColumns, ColumnToCheck, Movements) :-
	NextIteration is Iteration + 1,
	find_movements(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Movements).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

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
	Logica de fusion:
		Dada una grilla, se revisan los bloques adyacentes a cada bloque y se determina la cantidad
		de bloques iguales.
		En caso de hayar coincidencias, se compara cuales de estos tienen mas bloques iguales adyacentes.
		Al determinar esto, dado el bloque de mas posibles combinaciones, se fusiona culminando en la 
		posicion de dicho bloque.
		En caso de que varios bloques adyacentes puedan fusionar con la misma cantidad de bloques, 
		la fusion culmina en la posicion del bloque que se movio ultimo.
		En caso de que haya una unica fusion posible y esta sea un bloque arriba de otro, esto resulta en una 
		fusion en direccion hacia arriba.
	Proceso de fusion (shoot):
		El proceso de disparo consta de la etapa de insercion de bloque seguido de llevar a cabo todas las posibles
		fusiones, aplicando gravedad entre los pasos.

		Este proceso de fusiones se divide de la siguiente manera:
			(1) Dada una grilla, se recorre para saber que posiciones pueden fusionarse.
			Una vez decidido que bloques se van a fusionar:
				-(2) Se modifica el valor del bloque fusionado
				-(0) Dada la posicion donde se inserto el bloque nuevo, tengo que hacer check position a las celdas adyacentes
				a los lados si estas son iguales, y luego, de entre las opciones elijo la que mas fusiones admita. Si 
				el bloque disparado admite una sola fusion entonces fusiono hacia si mismo excepto que dicha fusion sea con
				el bloque de arriba, en cuyo caso fusiono hacia arriba.
				-(1.1) Se guarda la posicion de dicho bloque donde culmino la fusion (en una lista)
				-(3) Se reemplazan los bloques usados para la fusion por '-'
				-(block_fall) Luego de realizar todas las fusiones, se aplica gravedad a toda la grilla
			(1) Luego se vuelve a recorrer y si no hay mas posibles fusiones se da por terminado el shoot.
*/

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
check_Position(+Grid, +Col, +Index, -Matches, -MIndexes)
este predicado se encarga de, dada una grilla, la cantidad de columnas y una posicion, revisar
sus celdas adyacentes para saber cuantas son fusionables y cuales son sus posiciones
Matches es la cantidad de celdas adyacentes fusionables 
MIndexes son los indices de estas celdas fusionables
*/

check_position(Grid, Col, Index, Matches, MIndexes) :-
	valid_index(Grid, Index),
	check_right(Grid, Col, Index, RightMatch, RightIndex),
	check_left(Grid, Col, Index, LeftMatch, LeftIndex),
	check_top(Grid, Col, Index, TopMatch, TopIndex),
	check_bottom(Grid, Col, Index, BottomMatch, BottomIndex),
	append(LeftIndex, RightIndex, RLIndex),
	append(TopIndex, BottomIndex, TBIndex),
	append(RLIndex, TBIndex, MIndexes),
	Matches is RightMatch + LeftMatch + BottomMatch + TopMatch.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
valid_index(+Grid, +Index)
Este predicado verifica que el indice apunte a un numero y no a un guion
*/
valid_index(Grid, Index) :-
	nth0(Index, Grid, Element),
	Element \= '-'.
%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
check_right(+Grid, +Col, +Index, -Match, -BlockIndex)
Este predicado verifica si el bloque de la derecha coincide
*/
check_right(_Grid, Col, Index, 0, []) :-
	last_column(Col, Index), !.

check_right(Grid, _Col, Index, 1, MIndexes) :-
	RightBlockIndex is Index + 1,
	nth0(RightBlockIndex, Grid, RightBlockValue),
	nth0(Index, Grid, BlockValue),
	RightBlockValue == BlockValue, !,
	MIndexes = [RightBlockIndex].

check_right(Grid, _Col, Index, 0, []) :-
	RightBlockIndex is Index + 1,
	nth0(RightBlockIndex, Grid, RightBlockValue),
	nth0(Index, Grid, BlockValue),
	RightBlockValue \= BlockValue.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
check_left(+Grid, +Col, +Index, -Match, -BlockIndex)
Este predicado verifica si el bloque de la izquierda coincide
*/
check_left(_Grid, Col, Index, 0, []) :-
	first_column(Col, Index), !.

check_left(Grid, _Col, Index, 1, MIndexes) :-
	LeftBlockIndex is Index - 1,
	nth0(LeftBlockIndex, Grid, LeftBlockValue),
	nth0(Index, Grid, BlockValue),
	LeftBlockValue == BlockValue, !,
	MIndexes = [LeftBlockIndex].

check_left(Grid, _Col, Index, 0, []) :-
	LeftBlockIndex is Index - 1,
	nth0(LeftBlockIndex, Grid, LeftBlockValue),
	nth0(Index, Grid, BlockValue),
	LeftBlockValue \= BlockValue.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
check_bottom(+Grid, +Col, +Index, -Match, -BlockIndex)
Este predicado verifica si el bloque de abajo coincide
*/
check_bottom(_Grid, Col, Index, 0, []) :-
	last_row(Col, Index), !.

check_bottom(Grid, Col, Index, 1, MIndexes) :-
	BottomBlockIndex is Index + Col,
	nth0(BottomBlockIndex, Grid, BottomBlockValue),
	nth0(Index, Grid, BlockValue),
	BottomBlockValue == BlockValue, !,
	MIndexes = [BottomBlockIndex].

check_bottom(Grid, Col, Index, 0, []) :-
	BottomBlockIndex is Index + Col,
	nth0(BottomBlockIndex, Grid, BottomBlockValue),
	nth0(Index, Grid, BlockValue),
	BottomBlockValue \= BlockValue.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
check_top(+Grid, +Col, +Index, -Match, -BlockIndex)
Este predicado verifica si el bloque de arriba coincide
*/
check_top(_Grid, Col, Index, 0, []) :-
	first_row(Col, Index), !.

check_top(Grid, Col, Index, 1, MIndexes) :-
	TopBlockIndex is Index - Col,
	nth0(TopBlockIndex, Grid, TopBlockValue),
	nth0(Index, Grid, BlockValue),
	TopBlockValue == BlockValue, !,
	MIndexes = [TopBlockIndex].

check_top(Grid, Col, Index, 0, []) :-
	TopBlockIndex is Index - Col,
	nth0(TopBlockIndex, Grid, TopBlockValue),
	nth0(Index, Grid, BlockValue),
	TopBlockValue \= BlockValue.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/* 
first_column(+Col, +Index)
Este predicado verifica si la posicion pasada por 
parametro pertenece a la primera columna
*/

first_column(_Col, 0) :- !.
first_column(Col, Index) :-
	Index > 0,
	NextIndex is Index - Col,
	first_column(Col, NextIndex).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/* 
last_column(+Col, +Index)
Este predicado verifica si la posicion pasada por 
parametro pertenece a la ultima columna
*/

last_column(Col, Index) :- 
	Index is Col - 1, !.
last_column(Col, Index) :-
	Index > 0,
	NextIndex is Index - Col,
	last_column(Col, NextIndex).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/* 
first_row(+Col, +Index)
Este predicado verifica si la posicion pasada por 
parametro pertenece a la primera fila
*/
first_row(Col, Index) :-
	Index < Col.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/* 
last_row(+Col, +Index)
Este predicado verifica si la posicion pasada por 
parametro pertenece a la ultima fila
*/
last_row(Col, Index) :-
	gridSize(GLength),	
	TopRowMinIndex is GLength - Col,
	Index >= TopRowMinIndex.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/* 
new_block_value(+BlockValue, +Matches, -NewBlockValue)
Este predicado calcula el nuevo valor del bloque a fusionar, basado en el bloque y la 
cantidad de bloques con los que se va a fusionar.
*/

new_block_value(BlockValue, Matches, NewBlockValue) :-
	NewBlockValue is BlockValue * (2 ** Matches).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
fusion(+Grid, +Col, +Index, -FGrid, -NewBlock, -ResultIndex)
fusion se encarga de llevar a cabo las fusiones, puede dar falso
Retorna una grilla resultante, una estructura tipo newBlock con el nuevo bloque
y el indice de donde se termino la fusion
*/

%Caso 1: Hay una unica fusion posible, arriba
fusion(Grid, Col, Index, FGrid, newBlock(NewBlockValue), [BlockMatchIndex]) :-
	check_position(Grid, Col, Index, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, BestMatchIndex, BestMatchMerges, _BestMatchMergesIndexes),
	Matches == BestMatchMerges,
	Matches == 1,
	%Uso last porque si hay fusion posible arriba su posicion es la ultima
	last(MIndexes, BlockMatchIndex),
	%Verifico si ese unico match efectivamente esta arriba
	BlockMatchIndex is Index - Col, !,
	nth0(Index, Grid, BlockValue),
	new_block_value(BlockValue, Matches, NewBlockValue),
	replace_at_index(Grid, BlockMatchIndex, NewBlockValue, RGrid),
	replace_at_index(MIndexes, BestMatchIndex, Index, BlocksToRemoveIndexes),
	remove_merged(RGrid, BlocksToRemoveIndexes, FGrid).

%Caso 2: Caso merge sobre INDEX
fusion(Grid, Col, Index, FGrid, newBlock(NewBlockValue), [Index]) :-
	check_position(Grid, Col, Index, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, _BestMatchIndex, BestMatchMerges, _BestMatchMergesIndexes),
	Matches >= BestMatchMerges, !,
	nth0(Index, Grid, BlockValue),
	new_block_value(BlockValue, Matches, NewBlockValue),
	replace_at_index(Grid, Index, NewBlockValue, RGrid),
	remove_merged(RGrid, MIndexes, FGrid).

%Caso 3: Caso merge sobre la mejor opcion
fusion(Grid, Col, Index, FGrid, newBlock(NewBlockValue), [BestMatchIndexOnGrid]) :-
	check_position(Grid, Col, Index, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, BestMatchIndexOnMIndexes, BestMatchMerges, BestMatchMergesIndexes),
	Matches < BestMatchMerges,
	nth0(BestMatchIndexOnMIndexes, MIndexes, BestMatchIndexOnGrid),
	nth0(BestMatchIndexOnGrid, Grid, BlockValue),
	new_block_value(BlockValue, BestMatchMerges, NewBlockValue),
	replace_at_index(Grid, BestMatchIndexOnGrid, NewBlockValue, RGrid),
	remove_merged(RGrid, BestMatchMergesIndexes, FGrid).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------

/*
remove_merged(+Grid, +Indexes, -RGrid)
remove merged recibe una grilla y una lista de indices de bloques a remover
retorna una grilla donde todos los bloques a remover se reemplazaron por '-'
*/

remove_merged(Grid, [], Grid):- !.

remove_merged(Grid, [X | Xs], RGrid) :-
	replace_at_index(Grid, X, '-', NextGrid),
	remove_merged(NextGrid, Xs, RGrid).

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------
/*
best_match(+Grid, +Col, +MIndexes, -BestMatchIndex, -BestMatchMerges, -BestMatchMergesIndexes)
best match retorna la posicion donde mas fusiones se logran, la cantidad, y los indices de
los bloques con los que se logra esta fusion
*/

best_match(Grid, Col, MIndexes, BestMatchIndex, BestMatchMerges, BestMatchMergesIndexes) :-
	best_match_aux(Grid, Col, MIndexes, MatchMerges, MergesList),
	max_list(MatchMerges, BestMatchMerges),
	nth0(BestMatchIndex, MatchMerges, BestMatchMerges),
	nth0(BestMatchIndex, MergesList, BestMatchMergesIndexes), !.


/*
best_match_aux(+Grid, +Col, +MIndexes, -MatchMerges, -MergesList)
aux retorna dos listas:
	La primera es cuantas fusiones logra cada indice
	La segunda es una lista de con quienes (indices)
*/
best_match_aux(Grid, Col, [X], [Y], [MIndexes]) :-
	check_position(Grid, Col, X, Y, MIndexes), !.

best_match_aux(Grid, Col, [X], [_Y | Ys], [_Z | MIndexes]) :-
	check_position(Grid, Col, X, Ys, MIndexes), !.

best_match_aux(Grid, Col, [X | Xs], [Y | Ys], [MIndexes | MIndexes2]) :-
	check_position(Grid, Col, X, Y, MIndexes),
	best_match_aux(Grid, Col, Xs, Ys, MIndexes2), !.

%-------------------------------------------------------------------------------------------

%-------------------------------------------------------------------------------------------