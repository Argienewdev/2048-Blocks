# Proyecto: <span style="color:#e6538a">M2 Blocks</span>
*Lógica para Cs. de la Computación - 2025*

## El juego
El *M2 Blocks* (o *Merge 2 Blocks*) es un juego de puzzle y estrategia cuyo objetivo es lanzar y combinar bloques numerados para crear bloques de mayor valor y ganar puntos. Se encuentra disponible para Android y iOS: [m2blocks.fungame.studio](https://m2blocks.fungame.studio)

![Captura del juego M2 Blocks](/docs/images/m2blocks-game-screenshot.png)

## Requerimientos
### Funcionalidad
Se debe implementar una aplicación web que permita jugar al *M2 Blocks*, con una interfaz del estilo de las apps para Android y iOS. 

Principales funcionalidades a ser contempladas:

- <span style="color:#fc7f40">📌 **Generación aleatoria del bloque a disparar.**</span> Tener en cuenta que el número a disparar debe ser elegido aleatoriamente dentro de un rango permitido. Este rango permitido va variando a medida que evoluciona el juego y se van consiguiendo bloques más grandes:

  | **Máximo de la Grilla** | **Rango**       | **Observación**          |
  |-------------------------|-----------------|--------------------------|
  | 2, 4, 8                 | 2 a 4           |                          |
  | 16                      | 2 a 8           |                          |
  | 32                      | 2 a 16          |                          |
  | 64                      | 2 a 32          |                          |
  | 128, 256, 512           | 2 a 64          |                          |
  | 1024                    | 4 a 128         | Se retira el 2           |
  | 2048                    | 8 a 256         | Se retira el 4           |
  | 4096, 8192              | 16 a 512        | Se retira el 8           |
  | 16k                     | 32 a 1024       | Se retira el 16          |
  | …                       | …               | …                        |

- <span style="color:#fc7f40">📌  **Efecto del disparo de un bloque.**</span> Involucra la ubicación del bloque en la columna del disparo, posiblemente seguido de una serie de efectos en cadena alternando la mezcla de bloques adyacentes iguales, generando nuevos bloques, y “caídas” de bloques (pensándolo como gravedad invertida) para ocupar espacios que se originaron de las mezclas. La interfaz debe mostrar cada uno de estos efectos en cadena progresivamente, uno luego del otro, estilo animación. **Importante**: se debe imitar fielmente el comportamiento de la aplicación. Ante cualquier duda acerca de algún comportamiento específico, consultar con el docente asignado.
- <span style="color:#fc7f40">📌  **Avisos “*Combo x N*”.**</span> Por ejemplo: “Combo x 3” significa que se produjeron 3 mezclas de bloques como consecuencia del mismo disparo.
- <span style="color:#fc7f40">📌  **Avisos de nuevo bloque máximo logrado.**</span> Al menos a partir del 512. Cuando corresponda, por ej. al lograr el 1024, también avisos de bloque agregado al rango de disparo (“New block added”) y bloque “retirado” del rango de disparo (“Eliminated Block”).
- <span style="color:#fc7f40">📌  **Limpieza de bloques retirados.**</span> Esto es, que ya no se generan más. Cuando se logra un nuevo bloque máximo, y si esto implica que deja de generarse un determinado bloque (extremo inferior del rango actual), las apariciones de ese bloque, ahora “retirado”, deben eliminarse. Esta limpieza puede pensarse como un efecto más de la movida que causó el retiro del bloque.
- <span style="color:#fc7f40">📌  **Booster *Hint jugada*.**</span> Al activarlo muestra, para cada columna, una pista (por ejemplo, flotando sobre la propia columna, semi-transparente) del resultado que se va a conseguir con esa jugada, por ejemplo: *bloque X*, *combo x N*, etc. Puede activarse en cualquier momento, cuantas veces se quiera, y muestra las pistas solo para la jugada actual.
- <span style="color:#fc7f40">📌  **Booster *Bloque siguiente*.**</span> Al activarlo se muestra el bloque del disparo siguiente, además del actual. Puede activarse en cualquier momento, cuantas veces se quiera, y dura por un tiempo limitado.

<hr style="border-top:5px solid #0083bb; border-bottom: 0; margin: 40px 0;"/>

## Implementación molde en React + Prolog

Implementación molde a usar como punto de partida para la resolución del proyecto de la materia, usando React del lado del cliente para la UI, y Prolog del lado del servidor para la lógica del juego.

### Setup y ejecución del servidor Pengines
- [Descargar](https://www.swi-prolog.org/Download.html) e instalar el SWI-Prolog.

- Levantar el servidor ejecutando en SWI-Prolog el `run.pl` en la carpeta `pengines_server`: 

  `cd pengines_server`\
  `swipl run.pl`
  
  o haciendo doble click sobre el `run.pl`.

  Aclaración: no hacer `swipl pengines_server/run.pl` porque algunas referencias luego no funcionan.

  La primera vez que se ejecute el run.pl se pedirá definir un username y un password para acceder a la consola web admin del servidor, elegir cualquiera (por ejemplo, username: 'lcc' y password: 'lccdcic'), pero no dejar vacíos.

- El servidor escuchará en http://localhost:3030

- Ir a http://localhost:3030/admin/server.html para ver la consola web admin.

- La carpeta `pengines-master/apps/proylcc` contiene el código prolog del tic tac toe. Cada vez que se modifica este código es necesario bajar y volver a levantar el servidor para que se reflejen los cambios.

### Setup y ejecución de la aplicación React

- Descargar una versión reciente de [Node.js](https://nodejs.org/en/).

- Ejecutar 

  `npm install` 

  en el directorio del proyecto (`tic-tac-toe`) para instalar las dependencias (librerías)
localmente, en la carpeta `node_modules`.

- Ejecutar

    `npm start`

    en el directorio del proyecto para correr la app en modo desarrollo.

- Abrir [http://localhost:3000](http://localhost:3000) para ver la aplicación en el browser.

- La página se refresca automáticamente cuando cambia el código.
