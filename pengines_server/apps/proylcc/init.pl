:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */
/* 
*/


init([
	4,4,16,32,32,
	4,4,-,4,16,
	8,-,-,-,4,
	-,-,-,-,-,
	-,-,-,-,-,
	-,-,-,-,-,
	-,-,-,-,-
	], 5).
/*
init([
	1048576,2097152,8,64,32,
	2,524288,1024,4,16,
	-,-,-,4096,2,
	-,-,-,-,16,
	-,-,-,-,2,
	-,-,-,-,-,
	-,-,-,-,-
	], 5).
*/