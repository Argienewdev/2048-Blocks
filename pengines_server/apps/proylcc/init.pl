:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */
/* 

init([
	4,2,8,64,32,
	2,-,-,4,16,
	8,-,-,-,4,
	-,-,-,-,-,
	-,-,-,-,-,
	-,-,-,-,-,
	-,-,-,-,-
	], 5).
*/
init([
    2,4, 2, 4, 8,
    4,2, 4, 8, 4,
    2, 8, 2, -, -,
    8, 4, -, -, -,
    -, 8, -, -, -,
    -, -, -, -, -,
    -, -, -, -, -
    ], 5).
