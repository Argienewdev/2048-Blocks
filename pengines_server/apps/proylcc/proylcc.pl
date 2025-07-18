:- module(proylcc, 
	[  
		randomBlock/5,
		shoot/6,
		shootCache/7,
		booster_hint/4
	]).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).

:- dynamic gridSize/1.
%-------------------------------------------------------------------------------------------

/*
conditional_replace_at_index(+Index, +List, +OldElement, +NewElement, -ResultList)
Retorna true solo si existe OldElement en Index. Lo reemplaza por NewElement. Indice basado en 1
*/

conditional_replace_at_index(1, [OldElement|Tail], OldElement, NewElement, [NewElement|Tail]).

conditional_replace_at_index(Index, [Head|Tail], OldElement, NewElement, [Head|ResultTail]) :-
    Index > 1,
    NewIndex is Index - 1,
    conditional_replace_at_index(NewIndex, Tail, OldElement, NewElement, ResultTail).

%-------------------------------------------------------------------------------------------

/*
replace_at_index(+List, +Index, +Element, -ResultList)
Reemplaza un elemento por otro dado un indice en base 1.
*/

replace_at_index([_ | T], 0, Elem, [Elem | T]) :- !.

replace_at_index([H | T], Index, Elem, [H | R]) :-
    NextIndex is Index - 1,
    replace_at_index(T, NextIndex, Elem, R).

%-------------------------------------------------------------------------------------------

score('-').

%-------------------------------------------------------------------------------------------

/*
max_actual(+Grid, -Max)
Retorna el maximo actual de la grilla
Si la grilla estaba vacia, el maximo sera 0
*/

max_grid(Grid, 0):- 
	exclude(score, Grid, []), !.

max_grid(Grid, Max):- 
	exclude(score, Grid, Numeros), 
	max_list(Numeros, Max).

%-------------------------------------------------------------------------------------------

/*
min_actual(+Grid, -Min)
Retorna el minimo actual de la grilla
Como nunca se llama con la grilla vacia, no se verifica el caso
*/

min_actual(Grid, Min):- 
	exclude(score, Grid, Numeros), 
	min_list(Numeros, Min).

%-------------------------------------------------------------------------------------------

/*
min_max_grid_values_permited retorna el rengo menor y mayor segun grilla, luego segun formula.
min_max_grid_values_permited(MaxActual, MinPermitido, MaxPermitido)
*/

min_max_grid_values_permited(0, 2, 4) :- !.
min_max_grid_values_permited(Max, 2, 4) :- member(Max, [2,4,8]), !.
min_max_grid_values_permited(16, 2, 8) :- !.
min_max_grid_values_permited(32, 2, 16) :- !.
min_max_grid_values_permited(64, 2, 32) :- !.
min_max_grid_values_permited(Max, 2, 64) :- member(Max, [128,256,512]), !.
min_max_grid_values_permited(1024, 4, 128) :- !.
min_max_grid_values_permited(2048, 8, 256) :- !.
min_max_grid_values_permited(Max, 16, 512) :- member(Max, [4096,8192]), !.
min_max_grid_values_permited(16384, 32, 1024) :- !.

/* A partir de MaxAct >= 16384, el rango permitido (MinPerm y MaxPerm) se duplica
cada vez que MaxAct se duplica. Se usa K= piso de log(MaxAct / 16384) / log2 para contar cuántas
duplicaciones hubo respecto del valor base (16384), luego se incrementan K veces los exponentes.
*/
min_max_grid_values_permited(MaxAct, MinPerm, MaxPerm) :-
    Base is 16384,
    K is floor(log(MaxAct / Base) / log(2)),
    ExpMin is 5 + K,
    ExpMax is 10 + K,
    MinPerm is 2 ** ExpMin,
    MaxPerm is 2 ** ExpMax.

%-------------------------------------------------------------------------------------------

%Retorna true si se trata de una potencia de 2
es_potencia_de_dos(1). 

es_potencia_de_dos(N) :-
    integer(N),
    N > 1,
    0 is N mod 2,
    N2 is N // 2,
    es_potencia_de_dos(N2).

%-------------------------------------------------------------------------------------------

/*
remove_block(+Block, +Grid, -NewGrid, -Indexes)
Dada una grilla (Grid) y un valor de bloque (Block), reemplaza todos los elementos de la grilla
que sean numéricos y menores o iguales a Block por el guion '-', y retorna la grilla resultante (GridResult).
Esto se utiliza, por ejemplo, para eliminar todos los bloques de menor valor tras ciertas fusiones.
*/

remove_block(Block, Grid, NewGrid, Indexes) :-
    remove_block_aux(Block, '-', Grid, 0, NewGrid, Indexes).

remove_block_aux(_, _, [], _, [], []) :- !.

%Si es un numero y es menor o igual al bloque a eliminar, se elimina.
remove_block_aux(Block, Repl, [H|T], I, [Repl|NT], [I | Indexes]) :-
	number(H),
	Block >= H,
    I1 is I + 1, !,
    remove_block_aux(Block, Repl, T, I1, NT, Indexes).

%Si no es igual al bloque a eliminar se deja igual.
remove_block_aux(Block, Repl, [H|T], I, [H|NT], Indexes) :-
    Block \= H,
    I1 is I + 1,
    remove_block_aux(Block, Repl, T, I1, NT, Indexes).

%-------------------------------------------------------------------------------------------

% evaluate_elimination(+CurrentMax, +NewMax, -BlockToRemove)
% Este predicado determina si corresponde eliminar un bloque mínimo tras una jugada,
% y calcula cuál debe eliminarse según la lógica del juego.

evaluate_elimination(CurrentMax, NewMax, BlockToRemove) :-
    NewMax >= 1024,
    NewMax > CurrentMax,
    min_max_grid_values_permited(NewMax, BlockToRemoveBeforeDivision, _),
    BlockToRemove is BlockToRemoveBeforeDivision / 2.

%-------------------------------------------------------------------------------------------

/*
apply_deletes_and_merges(+Grid, +Col, +CurrentMax, +NewMax, -Effects, -RGrid, -Movements)
Este predicado, dada una grilla, evalua si es necesario eliminar un bloque
y en caso afirmativo:
	Elimina el bloque de la grilla
	Aplica gravedad en las columnas donde se eliminaron bloques

Se retorna:
	Los efectos
	La grilla final
	Los indices de los bloques que se movieron
*/

apply_deletes_and_merges(Grid, Col, CurrentMax, NewMax, Effects, RGrid, Movements, BlockToRemove) :-
    evaluate_elimination(CurrentMax, NewMax, BlockToRemove),
	remove_block(BlockToRemove, Grid, RemovedBlockGrid, RemovedIndexes),
	get_columns_to_check(RemovedIndexes, Col, ColumnsToCheck),
	% Obtiene todos los índices de la grilla resultante para aplicar gravedad
	(block_fall(RemovedBlockGrid, Col, ColumnsToCheck, GravityGrid, Movements) ->
	
	Effects = [effect(RemovedBlockGrid, []), effect(GravityGrid, [])],
	RGrid = GravityGrid;
	
	RGrid = RemovedBlockGrid,
	Movements = [],
	Effects = [effect(RemovedBlockGrid, [])]).


%-------------------------------------------------------------------------------------------

/*
randomBlock se encarga de, dada una grilla, retornar un numero aleatorio valido dadas las reglas del juego.
logica: elige el maximo valor actual de la grilla, y luego segun este valor calcula el rango minimo y maximo 
basandose en grilla o formula si el MaxAct>=16000.
por ultimo crea una lista de potencias de dos que esten dentro del rango y elige uno aleatorio.
*/

randomBlock(Grid, Block, MinRange, MaxRange, MaxAct):-
	max_grid(Grid, MaxAct), 
	min_max_grid_values_permited(MaxAct, MinRange, MaxRange),
	findall(X, (between(MinRange, MaxRange, X), es_potencia_de_dos(X)), Potencias), 
	random_member(Block, Potencias).

%-------------------------------------------------------------------------------------------

% shoot(+Block, +Grid, +NumCols, +Lane, -Hint, -Effects, -MaxRemovedBlock)
/*
Este shoot modificado debe permitir recabar toda la informacion relevante
La idea es que cada vez que se llame se pueda cachear en el front y permita
tener toda la informacion necesaria a disposicion
*/

shootCache(Block, Grid, NumCols, Lane, hint(Lane, Combo, MaxBlock), Effects, MaxRemovedBlock) :-
	shoot(Block, Lane, Grid, NumCols, Effects, MaxRemovedBlock),	% simulo el shoot (la jugada) en esa columna
	count_combo(Effects, Combo),									% cuento cuantas fusiones se produjeron
	max_newblock_from_effects(Effects, MaxBlock).					% encuentro el valor maximo producto de la fusion

%-------------------------------------------------------------------------------------------
%-------------------------------------------------------------------------------------------

% booster_hint(+Block, +Grid, +NumCols, -Hints)
%
% Dado un bloque Block, una grilla Grid y la cantidad de columnas NumCols,
% devuelve en Hints una lista con un hint por cada columna.
% Cada hint indica cuantas fusiones (newBlock) se producen
% si se tirara el bloque en esa columna.

booster_hint(Block, Grid, NumCols, Hints) :-
    findall(
	hint(Col, Combo, MaxBlock),							% por cada columna, armo un un par hint(Col, Combo, MaxBlock)
		(
		between(1, NumCols, Col),						% asigno los indices de las columnas
		shoot(Block, Col, Grid, NumCols, Effects, _),	% simulo el shoot (la jugada) en esa columna
		count_combo(Effects, Combo),					% cuento cuantas fusiones se produjeron
		max_newblock_from_effects(Effects, MaxBlock)	% encuentro el valor maximo producto de la fusion
		),
        Hints											% reunimos todos los hints en una lista
		).

%-------------------------------------------------------------------------------------------

% count_combo(+Effects, -Count)
%
% Dada la lista de Effects (como la que produce shoot/6),
% cuenta cuántos efectos contienen al menos una fusión newBlock(_).
% Ese número se devuelve como Count.

count_combo(Effects, Count) :-
    include(has_newblock, Effects, FusionEffects),	% me quedo solo con los que tienen fusiones
    length(FusionEffects, Count).					% contamos cuántos son

%-------------------------------------------------------------------------------------------

% has_newblock(+Effect)
%
% verdadero si el efecto dado contiene al menos un newBlock(_) en su lista de infos.

has_newblock(effect(_, Infos)) :-
    member(newBlock(_), Infos). % hay al menos una fusión

%-------------------------------------------------------------------------------------------

% max_newblock_from_effects(+Effects, -Max)
%
% Dada una lista de efectos 'Effects'
% devuelve en 'Max' el valor más alto de todos los bloques generados por fusiones
% Si no se generó ningún bloque, devuelve 0.

max_newblock_from_effects(Effects, Max) :-
    findall(V, (member(effect(_, Infos), Effects), member(newBlock(V), Infos)), Blocks),
    ( Blocks = [] -> Max = 0 ; max_list(Blocks, Max) ).

%-------------------------------------------------------------------------------------------
/*
shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects, -MaxRemovedBlock)
shoot en principio calcula la longitud de la grilla y la deja en assert para no volver a calcularla.
Luego, este procede del siguiente modo:
	block_insert: pone el bloque donde va luego de ser disparado, retorna efecto y a donde cayo
	fusion_process: lleva a cabo toda la serie de fusiones correspondientes y retorna los efectos
	append: junta los efectos para retornarlos

Retorna:
	La lista de efectos correspondientes
	El numero maximo eliminado de la grilla
*/

shoot(Block, Lane, Grid, Col, Effects, MaxRemovedBlock) :-
    gridSize(_), !,
    block_insert(Block, Lane, Grid, Col, InsertGrid, InsertIndex),
    fusion_process(InsertGrid, Col, [InsertIndex], FEffects, MaxRemovedBlock),
    append([effect(InsertGrid, [])], FEffects, Effects).

shoot(Block, Lane, Grid, Col, Effects, MaxRemovedBlock) :-
    length(Grid, GridSize),
    assert(gridSize(GridSize)),
    block_insert(Block, Lane, Grid, Col, InsertGrid, InsertIndex),
    fusion_process(InsertGrid, Col, [InsertIndex], FEffects, MaxRemovedBlock),
    append([effect(InsertGrid, [])], FEffects, Effects).

%-------------------------------------------------------------------------------------------

/*
fusion_process_aux(+Grid, +Col, +Indexes, -Effects)
Es un wrapper.
*/

fusion_process(Grid, Col, Indexes, Effects, MaxRemovedBlock) :-
	fusion_process_aux(Grid, Col, Indexes, Effects, 0, MaxRemovedBlock).
/*

%-------------------------------------------------------------------------------------------

fusion_process_aux(+Grid, +Col, +Indexes, -Effects, +RemovedBlockAcc -MaxRemovedBlock)
Este predicado se encarga de verificar si luego de las fusiones corresponde remover bloques en desuso.
En caso afirmativo, se llama recursivamente a si misma para corroborar si se pueden lograr nuevas fusiones
y revisa nuevamente si hay algun bloque para remover.
Luego, retorna los efectos pertinentes y el bloque mas grande eliminado.
*/


fusion_process_aux(Grid, Col, Indexes, Effects, RemovedBlockAcc, MaxRemovedBlock) :-
	fusion_admin(Grid, Col, Indexes, FusionEffects, FusionGrid),
	max_grid(Grid, CurrentMax),
	max_grid(FusionGrid, NewMax),
	(apply_deletes_and_merges(FusionGrid, Col, CurrentMax, NewMax, PostDeletesMergesEffects, PostDeletesMergesGrid, PostDeletesMergesMovements, RemovedBlock) ->

	append(FusionEffects, PostDeletesMergesEffects, NewEffects),
	fusion_process_aux(PostDeletesMergesGrid, Col, PostDeletesMergesMovements, NewNewEffects, RemovedBlock, MaxRemovedBlock),
	append(NewEffects, NewNewEffects,Effects);

	MaxRemovedBlock = RemovedBlockAcc,
	Effects = FusionEffects).

%-------------------------------------------------------------------------------------------

/*
fusion_admin_aux(+Grid, +Col, +Indexes, -Effects, -RGrid)
Es un wrapper.
*/

fusion_admin(Grid, Col, Indexes, FusionEffects, FusionGrid) :-
	fusion_admin_aux(Grid, Col, Indexes, [], FusionEffects, FusionGrid).

%-------------------------------------------------------------------------------------------
/*
fusion_admin_aux(+Grid, +Col, +Indexes, +Acc, -Effects, -RGrid)
Fusion admin funcionara de la siguiente manera
	fusion loop: se encarga de llevar a cabo todas las fusiones simultaneas y retorna el ultimo
	efecto junto con todos los bloques nuevos que se crearon y todos los nuevos indices de bloques
	que se movieron luego de las fusiones

	block fall: aplica gravedad, retorna la nueva grilla y los bloques que se movieron

	si block fall no era necesario, entonces solo me quedo con el efecto de las fusiones y
	hago un llamado recursivo para volver a fusion admin con los bloques que se movieron en las
	fusiones simultaneas

	si block fall si era necesario, agrego el efecto a la lista que voy a retornar, agrego los indices
	de bloques que se movieron a la lista de indices a revisar nuevamente y hago el llamado recursivo
*/

fusion_admin_aux(Grid, _Col, [], Acc, Acc, Grid) :- !.

fusion_admin_aux(Grid, Col, Indexes, Acc, Effects, RGrid) :-
	fusion_loop(Grid, Col, Indexes, FinalFusionEffect, LastGrid, NewFusionIndexes),
	get_columns_to_check_with_adyacent(NewFusionIndexes, Col, ColumnsToCheck),
	(block_fall(LastGrid, Col, ColumnsToCheck, GravityGrid, NewGravityIndexes) ->
	
	append(FinalFusionEffect, [effect(GravityGrid, [])], LoopEffect),
	append(Acc, LoopEffect, NewAcc),
	
	append(NewGravityIndexes, NewFusionIndexes, NewIndexes),
	
	fusion_admin_aux(GravityGrid, Col, NewIndexes, NewAcc, Effects, RGrid);
	
	append(Acc, FinalFusionEffect, NewAcc),
	
	fusion_admin_aux(LastGrid, Col, NewFusionIndexes, NewAcc, Effects, RGrid)), !.

%-------------------------------------------------------------------------------------------

/*
fusion_loop(+Grid, +Col, +Indexes, -FinalFusionEffect, -LastGrid, -NewIndexes)
fusion loop usa auxiliares para otorgar acumuladores
*/

fusion_loop(Grid, Col, Indexes, [], LastGrid, NewIndexes) :-
	fusion_loop_aux(Grid, Col, Indexes, effect(Grid, _), LastGrid, [], [], NewIndexes), !.

fusion_loop(Grid, Col, Indexes, [FinalFusionEffect], LastGrid, NewIndexes) :-
	fusion_loop_aux(Grid, Col, Indexes, FinalFusionEffect, LastGrid, [], [], NewIndexes).

/*
fusion_loop_aux(+Grid, +Col, +Indexes, -FinalFusionEffect, -LastGrid, +NewBlocksAcc, +NewIndexesAcc, -NewIndexes)
fusion loop aux funciona de la siguiente manera:
	Para cada indice a revisar, se lleva a cabo una fusion.
	Si esta fusion es exitosa, se almacena el indice resultante y los nuevos bloques formados.
	Luego se llama recursivamente hasta que no hayan mas indices para revisar.

	En caso de que la fusion falle, se desecha dicho indice y se sigue adelante con el resto.
*/

fusion_loop_aux(Grid, _Col, [], effect(Grid, Acc), Grid, Acc, NewIndexesAcc, NewIndexesAcc) :- !.

/*
En caso de que no haya fusion, da falso, entonces necesito que en ese
caso deseche el indice y siga, por eso se hace esta implicacion. Sino, deberia evaluar por falla
al predicado fusion, lo que seria una perdida de tiempo porque ya fue evaluado.
*/

fusion_loop_aux(Grid, Col, [X | Xs], FinalFusionEffect, LastGrid, Acc, NewIndexesAcc, NewIndexes) :-
	(fusion(Grid, Col, X, FGrid, NewIndexesAcc, NewBlocksAfterFusion, NewIndex) ->
	append(NewIndex, NewIndexesAcc, NextNewIndexesAcc),
	append([NewBlocksAfterFusion], Acc, NewAcc),
	fusion_loop_aux(FGrid, Col, Xs, FinalFusionEffect, LastGrid, NewAcc, NextNewIndexesAcc, NewIndexes);

	fusion_loop_aux(Grid, Col, Xs, FinalFusionEffect, LastGrid, Acc, NewIndexesAcc, NewIndexes)).

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

/*
block_fall(+Grid, +Col, +ColumnsToCheck, -GravityGrid, -Movements)
Este predicado implementa la gravedad, la cual hace que los bloques de la columna donde esta index y las 
adyacentes caigan.
	Predicados auxiliares:
		-Process_columns
			-Process_columns_aux
				-extract_column
					-nth0 (Libreria) Retorna un elemento dado un indice en base 0
				-sort_column
					-separate_scores
				-find_movements
				-reinsert_column
*/

%Si no hay movimientos, falla.
block_fall(Grid, Col, Indexes, GridOut, _Movements) :-
	block_fall_aux(Grid, Col, Indexes, GridOut, [], []), !, fail.

%Si hubo movimientos, retorna la grilla resultante y los indices de los movimientos.
block_fall(Grid, Col, Indexes, GridOut, Movements) :-
	block_fall_aux(Grid, Col, Indexes, GridOut, [], Movements).

%-------------------------------------------------------------------------------------------

%Si me quede sin columnas para revisar, devuelvo los movimientos y la grilla resultante.
block_fall_aux(Grid, _Col, [], Grid, Acc, Acc) :- !.

%Si tengo columnas para revisar, aplico gravedad, combino listas de movimientos y sigo recursivamente.
block_fall_aux(Grid, Col, [X | Xs], GridOut, Acc, Movements):-
	process_column(Grid, Col, X, NewGrid, XMovements),
	append(Acc, XMovements, NewAcc),
	block_fall_aux(NewGrid, Col, Xs, GridOut, NewAcc, Movements).

%-------------------------------------------------------------------------------------------
/*
get_columns_to_check(+Indexes, +TotalCols, -ColumnsToCheck)
Es un wrapper.
*/

get_columns_to_check([], _TotalCols, []) :- !.

get_columns_to_check(Indexes, TotalCols, ColumnsToCheck) :-
	get_columns_to_check_aux(Indexes, TotalCols, [], ColumnsToCheck).
	
%-------------------------------------------------------------------------------------------
/*
get_columns_to_check(+Indexes, +TotalCols, -ColumnsToCheck)
Es wrapper pero tambien agrega las columnas adyacentes de todas las encontradas.
*/

get_columns_to_check_with_adyacent([], _TotalCols, []) :- !.

get_columns_to_check_with_adyacent(Indexes, TotalCols, ColumnsToCheck) :-
	get_columns_to_check_aux(Indexes, TotalCols, [], PreColumnsToCheck),
	add_adyacent_columns(PreColumnsToCheck, TotalCols, ColumnsToCheck).
	
%-------------------------------------------------------------------------------------------

/*
get_columns_to_check_aux(+Indexes, +TotalColumns, +Acc, -ColumsToCheck)
Este predicado recibe una lista de indices donde se hicieron fusiones y retorna un conjunto
de columnas que incluyen:
	-Las columnas donde se hicieron fusiones, sin repeticiones.
*/

get_columns_to_check_aux([], _TotalColumns, Acc, Acc) :- !.

get_columns_to_check_aux([X | Xs], TotalColumns, Acc, ColumnsToCheck) :-
	calculate_column(TotalColumns, X, Column),
	(\+ member(Column, Acc) ->
		append(Acc, [Column], NewAcc),
		get_columns_to_check_aux(Xs, TotalColumns, NewAcc, ColumnsToCheck);

		get_columns_to_check_aux(Xs, TotalColumns, Acc, ColumnsToCheck)).

%-------------------------------------------------------------------------------------------
/*
add_adyacent_columns(+PreColumnsToCheck, +TotalColumns, -ColumnsToCheck)
Es un wrapper
*/

add_adyacent_columns(PreColumnsToCheck, TotalColumns, ColumnsToCheck) :-
	add_adyacent_columns_aux(PreColumnsToCheck, TotalColumns, PreColumnsToCheck, ColumnsToCheck).

%-------------------------------------------------------------------------------------------
/*
add_adyacent_columns_aux(+PreColumnsToCheck, +TotalColumns, +Acc, -ColumnsToCheck)
Dados los indices de las columnas a las que aplicar gravedad, se agregan las columnas adyacentes
de todos estos indices y luego se verifica por posibles duplicados para removerlos.
*/

add_adyacent_columns_aux([], _TotalColumns, Acc, NewAcc) :- 
	remove_duplicates(Acc, NewAcc), !.

add_adyacent_columns_aux([X | Xs], TotalColumns, Acc, ColumnsToCheck) :-
	add_left_column(X, Acc, NewAcc),
	add_right_column(TotalColumns, X, NewAcc, NewNewAcc),
	add_adyacent_columns_aux(Xs, TotalColumns, NewNewAcc, ColumnsToCheck).

%-------------------------------------------------------------------------------------------
/*
remove_duplicates(+List, -RList)
Dada una lista, la retorna sin duplicados.
*/

remove_duplicates([], []).

remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T),           % H no aparece en el resto
    remove_duplicates(T, Result).

remove_duplicates([H|T], Result) :-
    member(H, T),             % H aparece de nuevo más adelante
    remove_duplicates(T, Result).

%-------------------------------------------------------------------------------------------
/*
calculate_column(+TotalColumns, +Index, -ColumnIndex)
Dado un indice y la cantidad de columnas de la grilla, retorna el indice de la columna
a la que pertenece dicho indice.
*/

calculate_column(TotalColumns, Index, Index) :-
	Index < TotalColumns, !.

calculate_column(TotalColumns, Index, ColumnIndex) :-
	Index >= TotalColumns, !,
	ReducedIndex is Index - TotalColumns,
	calculate_column(TotalColumns, ReducedIndex, ColumnIndex).

%-------------------------------------------------------------------------------------------
/*
add_left_column(+TotalColumns, +ColIndex, +List, -RList)
Dado el indice de una columna de la grilla y una lista que modificar, se agrega
a dicha lista el indice de la columna a la izquierda del indice recibido.
*/

add_left_column(ColIndex, List, List) :-
	ColIndex =:= 0, !.

add_left_column(ColIndex, List, RList) :-
	LeftColumn is ColIndex - 1,
	LeftColumn >= 0, !,
	append(List, [LeftColumn], RList).

%-------------------------------------------------------------------------------------------
/*
add_right_column(+TotalColumns, +ColIndex, +List, -RList)
Dado el indice de una columna de la grilla y una lista que modificar, se agrega
a dicha lista el indice de la columna a la derecha del indice recibido.
*/

add_right_column(TotalColumns, ColIndex, List, List) :-
	RightColumn is ColIndex + 1,
	RightColumn =:= TotalColumns, !.

add_right_column(TotalColumns, ColIndex, List, RList) :-
	RightColumn is ColIndex + 1,
	ColIndex < TotalColumns, !,
	append(List, [RightColumn], RList).

%-------------------------------------------------------------------------------------------

/*
process_column(+GridIn, +TotalCols, -GridOut, -Movements)
process_column recorre la grilla extrayendo, organizando y reinsertando la columna indicada por ColToCheck
para que los bloques no tengan espacio vacio "debajo", retornando una nueva grilla sin bloques "flotantes".
*/

process_column(GridIn, TotalCols, ColToCheck, GridOut, Movements) :-
	process_columns_aux(ColToCheck, TotalCols, GridIn, GridOut, Movements).

process_columns_aux(ColToCheck, TotalCols, GridIn, GridOut, Movements) :-

    extract_column(GridIn, ColToCheck, TotalCols, Column),
    sort_column(Column, SortedColumn),
	find_movements(Column, SortedColumn, TotalCols, ColToCheck, Movements),
    reinsert_column(GridIn, ColToCheck, TotalCols, SortedColumn, GridOut).

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

/*
extract_column(+Grid, +ColIndex, +NumCols, -ExtractedColumn)
Este wrapper invoca al auxiliar con la fila inicializada en 0.

extract_column_aux(+Grid, +ColIndex, +NumCols, +Row, -ExtractedColumn)
extract_column_aux calcula que posiciones de la grilla pertenecen a la columna que se le 
pide extraer para luego agregarlas a la lista a retornar, la cual contiene todos los 
elementos de la columna especificada.
*/

extract_column(Grid, ColIndex, NumCols, ExtractedColumn) :- 
	extract_column_aux(Grid, ColIndex, NumCols, 0, ExtractedColumn).

extract_column_aux(Grid, ColIndex, NumCols, Row, [Elem]) :- 
	gridSize(GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
	nth0(Index, Grid, Elem), !.

extract_column_aux(Grid, ColIndex, NumCols, NumRows, [Elem | Rest]) :-
    Index is ColIndex + NumRows * NumCols,
    nth0(Index, Grid, Elem),
    NextNumRows is NumRows + 1,
    extract_column_aux(Grid, ColIndex, NumCols, NextNumRows, Rest).

%-------------------------------------------------------------------------------------------
/*
reinsert_column(+GridIn, +ColIndex, +NumCols, +ColumnToInsert, -GridOut)
Es un wrapper
*/

reinsert_column(GridIn, ColIndex, NumCols, ColumnToInsert, GridOut) :-
	reinsert_column_aux(GridIn, ColIndex, NumCols, 0, ColumnToInsert, GridOut).

%-------------------------------------------------------------------------------------------

/*
reinsert_column(+GridIn, +ColIndex, +NumCols, +ColumnToInsert, -GridOut)
Es un wrapper
reinsert_column_aux(+GridIn, +ColIndex, +NumCols, +Row, +ColumnToInsert, -GridOut)
Este predicado, dada una columna a reinsertar, la grilla, y el indice de dicha columna (basado en 0),
reemplaza los elementos que pertenezcan a la columna en la grilla por los de la nueva columna.
Este predicado comienza por la columna 0 y termina al llegar a la ultima columna.
*/

reinsert_column_aux(GridIn, ColIndex, NumCols, Row, [Elem], GridOut) :-
	gridSize(GridLength), 
	DRow is (GridLength / NumCols) - 1,
	DRow =:= Row,
	Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridOut), !.

reinsert_column_aux(GridIn, ColIndex, NumCols, Row, [Elem | Rest], GridOut) :-
    Index is ColIndex + Row * NumCols,
    replace_at_index(GridIn, Index, Elem, GridNext),
    NextRow is Row + 1,
    reinsert_column_aux(GridNext, ColIndex, NumCols, NextRow, Rest, GridOut).

%-------------------------------------------------------------------------------------------
/*
find_movements(+Column, +SortedColumn, +TotalColumns, +ColumnToCheck, -Movements)
Es un wrapper
*/

find_movements(Column, SortedColumn, TotalCols, ColToCheck, Movements) :-
	find_movements_aux(0, Column, SortedColumn, TotalCols, ColToCheck, Movements).

%-------------------------------------------------------------------------------------------

/*
find_movements(+Iteration, +Column, +SortedColumn, +TotalColumns, +ColumnToCheck, -Movements)
Este predicado revisa, dada una columna sin organizar y una organizada, que bloques se movieron
y a donde. Retorna a que posicion de la grilla se movieron.
*/

find_movements_aux(_Iteration, [], [], _TotalColumns, _ColumnToCheck, []):- !.

find_movements_aux(Iteration, ['-' | Xs], [Y | Ys], TotalColumns, ColumnToCheck, [IndexOnGrid | Zs]) :-
	Y \= '-',
	IndexOnGrid is ColumnToCheck + (Iteration * TotalColumns),
	NextIteration is Iteration + 1,
	find_movements_aux(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Zs), !.
	
find_movements_aux(Iteration, [X | Xs], [Y | Ys], TotalColumns, ColumnToCheck, [IndexOnGrid | Zs]) :-
	X \= Y,
	Y \= '-',
	IndexOnGrid is ColumnToCheck + (Iteration * TotalColumns),
	NextIteration is Iteration + 1,
	find_movements_aux(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Zs), !.

find_movements_aux(Iteration, [_X | Xs], [_Y | Ys], TotalColumns, ColumnToCheck, Movements) :-
	NextIteration is Iteration + 1,
	find_movements_aux(NextIteration, Xs, Ys, TotalColumns, ColumnToCheck, Movements).

%-------------------------------------------------------------------------------------------

/*
check_Position(+Grid, +Col, +Index, +InvalidIndexes, -Matches, -MIndexes)
este predicado se encarga de, dada una grilla, la cantidad de columnas y una posicion, revisar
sus celdas adyacentes para saber cuantas son fusionables y cuales son sus posiciones
Matches es la cantidad de celdas adyacentes fusionables
 
MIndexes son los indices de estas celdas fusionables

InvalidIndexes son indices de bloques no habilitados para fusion
*/

check_position(Grid, Col, Index, InvalidIndexes, Matches, MIndexesResult) :-
	valid_index(Grid, Index),
	check_right(Grid, Col, Index, RightMatch, RightIndex),
	check_left(Grid, Col, Index, LeftMatch, LeftIndex),
	check_top(Grid, Col, Index, TopMatch, TopIndex),
	check_bottom(Grid, Col, Index, BottomMatch, BottomIndex),
	append(LeftIndex, RightIndex, RLIndex),
	append(TopIndex, BottomIndex, TBIndex),
	append(RLIndex, TBIndex, MIndexes),
	diff_set(MIndexes, InvalidIndexes, MIndexesResult, Removed),
	Matches is RightMatch + LeftMatch + BottomMatch + TopMatch - Removed.

%-------------------------------------------------------------------------------------------

/*
diff_set(+List1, +List2, -Result, -RemovedCounter)
Este predicado encuentra la diferencia entre dos conjuntos y retorna la cantidad
de elementos que eran comunes entre los dos.
Su uso sera desechar indices invalidos de una lista de indices en principio
hayados y considerados validos.
*/

diff_set([], _, [], 0) :- !.

diff_set([X|Xs], Ys, [X|RList], Removed) :-
	\+ member(X, Ys), !, % Si X no está en Ys, se conserva
	diff_set(Xs, Ys, RList, Removed).

diff_set([X|Xs], Ys, RList, Removed) :-
    member(X, Ys), !, % Si X está en Ys, se elimina
    diff_set(Xs, Ys, RList, RecursiveRemoved),
    Removed is RecursiveRemoved + 1.

%-------------------------------------------------------------------------------------------

/*
valid_index(+Grid, +Index)
Este predicado verifica que el indice apunte a un numero y no a un guion
*/
valid_index(Grid, Index) :-
	nth0(Index, Grid, Element),
	Element \= '-'.
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
/* 
first_row(+Col, +Index)
Este predicado verifica si la posicion pasada por 
parametro pertenece a la primera fila
*/
first_row(Col, Index) :-
	Index < Col.

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

/* 
new_block_value(+BlockValue, +Matches, -NewBlockValue)
Este predicado calcula el nuevo valor del bloque a fusionar, basado en el bloque y la 
cantidad de bloques con los que se va a fusionar.
*/

new_block_value(BlockValue, Matches, NewBlockValue) :- 
	NewBlockValue is BlockValue * (2 ** Matches).

%-------------------------------------------------------------------------------------------
/*
fusion(+Grid, +Col, +Index, +InvalidIndexes,-FGrid, -NewBlock, -ResultIndex)
fusion se encarga de llevar a cabo las fusiones, puede dar falso
Retorna una grilla resultante, una estructura tipo newBlock con el nuevo bloque
y el indice de donde se termino la fusion

InvalidIndexes es una lista de indices de bloques que fueron creados en el mismo
ciclo de fusiones, por lo que no tienen que ser tomados en cuenta como validos para fusion.
*/

%Caso 1: Hay una unica fusion posible, arriba
fusion(Grid, Col, Index, FGrid, InvalidIndexes, newBlock(NewBlockValue), [BlockMatchIndex]) :-
	check_position(Grid, Col, Index, InvalidIndexes, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, InvalidIndexes, BestMatchIndex, BestMatchMerges, _BestMatchMergesIndexes),
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
fusion(Grid, Col, Index, FGrid, InvalidIndexes, newBlock(NewBlockValue), [Index]) :-
	check_position(Grid, Col, Index, InvalidIndexes, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, InvalidIndexes, _BestMatchIndex, BestMatchMerges, _BestMatchMergesIndexes),
	Matches >= BestMatchMerges, !,
	nth0(Index, Grid, BlockValue),
	new_block_value(BlockValue, Matches, NewBlockValue),
	replace_at_index(Grid, Index, NewBlockValue, RGrid),
	remove_merged(RGrid, MIndexes, FGrid).

%Caso 3: Caso merge sobre la mejor opcion
fusion(Grid, Col, Index, FGrid, InvalidIndexes, newBlock(NewBlockValue), [BestMatchIndexOnGrid]) :-
	check_position(Grid, Col, Index, InvalidIndexes, Matches, MIndexes),
	best_match(Grid, Col, MIndexes, InvalidIndexes, BestMatchIndexOnMIndexes, BestMatchMerges, BestMatchMergesIndexes),
	Matches < BestMatchMerges, !,
	nth0(BestMatchIndexOnMIndexes, MIndexes, BestMatchIndexOnGrid),
	nth0(BestMatchIndexOnGrid, Grid, BlockValue),
	new_block_value(BlockValue, BestMatchMerges, NewBlockValue),
	replace_at_index(Grid, BestMatchIndexOnGrid, NewBlockValue, RGrid),
	remove_merged(RGrid, BestMatchMergesIndexes, FGrid).

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
/*
best_match(+Grid, +Col, +MIndexes, +InvalidIndexes, -BestMatchIndex, -BestMatchMerges, -BestMatchMergesIndexes)
best match retorna la posicion donde mas fusiones se logran, la cantidad, y los indices de
los bloques con los que se logra esta fusion
*/

best_match(Grid, Col, MIndexes, InvalidIndexes, BestMatchIndex, BestMatchMerges, BestMatchMergesIndexes) :-
	best_match_aux(Grid, Col, MIndexes, InvalidIndexes, MatchMerges, MergesList),
	max_list(MatchMerges, BestMatchMerges),
	nth0(BestMatchIndex, MatchMerges, BestMatchMerges),
	nth0(BestMatchIndex, MergesList, BestMatchMergesIndexes), !.

%-------------------------------------------------------------------------------------------

/*
best_match_aux(+Grid, +Col, +MIndexes, +InvalidIndexes, -MatchMerges, -MergesList)
aux retorna dos listas:
	La primera es cuantas fusiones logra cada indice
	La segunda es una lista de con quienes (indices)
*/
best_match_aux(Grid, Col, [X], InvalidIndexes, [Y], [MIndexes]) :-
	check_position(Grid, Col, X, InvalidIndexes, Y, MIndexes), !.

best_match_aux(Grid, Col, [X], InvalidIndexes, [_Y | Ys], [_Z | MIndexes]) :-
	check_position(Grid, Col, X, InvalidIndexes, Ys, MIndexes), !.

best_match_aux(Grid, Col, [X | Xs], InvalidIndexes, [Y | Ys], [MIndexes | MIndexes2]) :-
	check_position(Grid, Col, X, InvalidIndexes, Y, MIndexes),
	best_match_aux(Grid, Col, Xs, InvalidIndexes, Ys, MIndexes2), !.

%------------------------------------------------------------------------------------------