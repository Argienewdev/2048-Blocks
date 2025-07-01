# Informe de la Resolución del Proyecto
*M2 Blocks - Lógica para Cs. de la Computación - 2025*

**Comisión `46`**:
- `Salamanca, Santiago`
- `Santa María, Joaquín`
---
Dadas las principales funcionalidades a ser contempladas tal como estan presentadas en el repositorio de GitHub, a continuacion se describirá como se implementaron.
    
- <span style="color:#fc7f40">📌**Generación aleatoria del bloque a disparar.**</span> 

    Para determinar el valor del próximo bloque, se utiliza un predicado que recibe la grilla actual y calcula un valor aleatorio válido según las reglas del juego.

    El proceso comienza identificando el valor máximo actual de la grilla. En función de este valor, se define un rango mínimo y máximo dentro del cual se permite generar el siguiente bloque.

    En las primeras etapas de la partida, el rango se establece mediante condiciones fijas basadas en la tabla proporcionada por la catedra en github. Sin embargo, cuando el valor máximo supera los 16.000, se aplica una fórmula específica que ajusta el rango de forma proporcional al avance del juego.

    Esta fórmula calcula la cantidad de veces que el valor máximo se ha duplicado respecto de 16.000. La cantidad de duplicaciones se obtiene utilizando el logaritmo base 2. A partir de este valor, se determina el rango permitido según las siguientes expresiones:

    Mínimo permitido: 2<sup>5+K</sup>  

    Máximo permitido: 2<sup>10+K</sup>

    Donde K representa la cantidad de duplicaciones del valor máximo actual (MaxAct) respecto de 16.000, calculada como:

    K = floor(log<sub>2</sub>(MaxAct / 16000))

    De esta manera, por cada duplicación del valor máximo, el rango permitido se desplaza a valores más altos de forma controlada y progresiva.

    Una vez determinado el rango, se filtran todas las potencias de dos dentro de esos límites y se selecciona aleatoriamente una de ellas como próximo bloque a disparar.

- <span style="color:#fc7f40">📌  **Efecto del disparo de un bloque.**</span> 

    Como parte de las optimizaciones implementadas, se tuvo en cuenta el costo de calcular repetidamente la longitud de la grilla (mediante `length/2`) y la redundancia de que multiples predicados realicen el calculo agregando lineas de codigo que podrian omitirse. Para evitar este sobrecosto, se definieron dos variantes del predicado `shoot`:

    - Una primera versión se activa únicamente la primera vez que se invoca el disparo. En esta se calcula la longitud total de la grilla y se utiliza `assert/1` para guardar este valor como un hecho dinámico en la base de conocimiento. Luego, procede con su normal funcionamiento.

    - A partir de entonces, una segunda versión de `shoot` se encarga de verificar si ese hecho ya existe. Si es así, procede normalmente con `shoot`.

    Esta técnica permite evitar cálculos redundantes sin afectar el comportamiento lógico del juego, mejorando ligeramente la eficiencia sin introducir complejidad significativa.

    Para resolver el efecto de disparo de un bloque, el problema fue subdividido en tareas individuales que, combinadas, permiten alcanzar el objetivo. Esta fragmentación también permitió la reutilización de predicados en otras partes del juego.

    Primero, se diseñó un predicado encargado de insertar el bloque recién disparado en la grilla. Esto significa que, dado el bloque y la columna de disparo, el predicado lo ubica en el último espacio vacío de esa columna antes de encontrarse con un bloque existente o el final de la grilla.

    Este proceso se realiza sobre la grilla, que está representada como una lista unidimensional. Dada la cantidad de columnas y la columna seleccionada, se calculan los índices correspondientes a esa columna. Luego se recorren esos índices, desde los más bajos hasta los más altos, buscando una celda vacía (representada por un guión). Si no hay espacio disponible, el predicado no realiza cambios. Si logra insertarlo, se retorna la nueva grilla junto con el índice de inserción.

    Una vez insertado el bloque, entra en juego el predicado más importante del sistema: el encargado de identificar y ejecutar las fusiones disponibles, respetando las reglas del juego original.

    ### 🔹 Estructura jerárquica de resolución de fusiones

    El núcleo de la lógica del juego se basa en cuatro niveles de predicados encadenados de forma jerárquica:

    #### 1. `process`:

    Su función es controlar la existencia de bloques en desuso luego de realizar las fusiones correspondientes. Esta etapa tiene un comportamiento recursivo que asegura una limpieza total de la grilla antes de retornar los efectos finales.

    Sus tareas principales son:

    - Invocar a admin para procesar las fusiones y aplicar la gravedad.

    - Verificar si, tras ese proceso, existen bloques que deban ser removidos. La descripción de esta funcionalidad se encuentra detallada más adelante en el informe.

    - En caso afirmativo, realizar dicha remoción y llamarse recursivamente a sí mismo para evaluar si surgen nuevas oportunidades de fusión.

    - Repetir este ciclo hasta que no haya más bloques en desuso.

    - Retornar la grilla final y los efectos acumulados.

    Este enfoque garantiza una limpieza completa de bloques que ya no forman parte de ninguna combinación válida, incluso si su remoción desencadena nuevas fusiones posibles y posteriormente la necesidad de remover mas bloques.
    
    > Como técnica de optimización, se utilizó la implicación (**`->`**) para evitar reevaluaciones innecesarias cuando la busqueda de bloques para eliminar resulta negativa.

    #### 2. `admin`:

    Controla el ciclo completo del disparo de un bloque. Se encarga de:

    - Invocar a `bucle` para identificar fusiones iniciales  a partir del índice del nuevo bloque.
    - Aplicar la gravedad una vez que se realizaron fusiones.
    - Invocar nuevamente `bucle` para revisar nuevas posibles fusiones  que puedan haber surgido tras la caída de bloques y fusiones previas.

    > Como técnica de optimización, se utilizó la implicación (**`->`**) para evitar reevaluaciones innecesarias cuando aplicar gravedad no provoca ningun cambio.

    #### 3. `bucle`:

    Se encarga de recorrer una lista de índices a revisar y, para cada uno, intentar ejecutar una fusión con `fusion`.\
    Esta etapa nace a partir de una decisión de diseño clave: los bloques formados en una fusión no deben ser candidatos a otra fusión dentro del mismo ciclo.

    **Ejemplo:**

    ```
    [4 2 2  →   [4 4 -  →   [8 4 -
     4 - -]      4 - -]      - - -]
    ```

    En este ejemplo, el nuevo `4` formado no puede fusionarse con los otros     `4`, ya que todos deberían haberse fusionado simultáneamente, lo cual   no es viable sin paralelismo. Por eso, se ideó el concepto de "ciclo"     de fusiones. Las fusiones dentro de un mismo ciclo solo consideran  bloques anteriores, y sus resultados se usan en el siguiente.

    En cuanto al flujo, `bucle` itera sobre cada índice a revisar. Llama a  `fusion`, y si esta tiene éxito, guarda el índice donde terminó. Si  falla, descarta el índice y continúa.

    > Como técnica de optimización, se utilizó la implicación (**`->`**) para evitar reevaluaciones innecesarias cuando una fusión no es posible.

    Una vez recorridos todos los índices, `bucle` retorna:

    - Los índices finales de las fusiones realizadas
    - La grilla resultante
    - La lista de nuevos bloques `newblock` formados

    Los nuevos indices serán utilizados por `admin` para aplicar relanzar el ciclo. La grilla resultante y los nuevos bloques formados serán utilizados para la creacion de efectos.

    #### 4. `fusion`:

    Es el núcleo de la lógica de combinación de bloques. A pesar de ser el último paso de la fusión, está compuesto por varios subpredicados.

    Recibe (entre otras cosas): 
    - El índice a revisar
    - Una lista de bloques "inválidos" (formados en el mismo ciclo) 

    Su tarea es:    
    1. Verificar si el bloque en ese índice puede fusionarse con algún  adyacente (predicado aparte).
    2. Evaluar si hay algún adyacente que sea mejor candidato para la   fusión, según los posibles resultados futuros (predicado aparte).
    3. Considerar las reglas específicas del juego original:
       - Si solo puede fusionar con un bloque "arriba", la fusión se    hace hacia arriba.
       - Si hay un mejor candidato adyacente que produce una mejor  fusión, se prioriza ese bloque.  
 
    Finalmente, retorna:    
    - El índice donde terminó la fusión
    - El nuevo bloque `newblock`
    - La grilla modificada
     
    Luego de completar todas las fusiones posibles, se procede a revisar si se generó un nuevo bloque máximo en la grilla y se eliminan los  bloques que ya no están en uso. La descripción de este proceso se encuentra más adelante en el informe.

    ---

    ### 🔽 Aplicación de la gravedad

    Una vez concluidas las fusiones en una grilla, es posible que queden espacios vacíos entre bloques, debido a que algunas celdas fueron liberadas tras las combinaciones. Para simular el efecto de la gravedad, se diseñó un mecanismo que reorganiza los bloques de las columnas, haciendo que los espacios vacíos se acumulen en los índices superiores de la grilla, es decir, en la parte inferior de la pantalla.

    Esto se logra recibiendo los índices de las columnas que deben ser  reorganizadas. Dichas columnas se obtienen a partir de un predicado que  utiliza los índices donde culminaron las fusiones, calcula a qué columnas pertenecen y agrega también las columnas adyacentes, todo esto sin repeticiones.

    Una vez identificadas las columnas a reorganizar, se extraen    individualmente de la grilla, se reordenan y luego se reinsertan en su posición original.

    Este procedimiento se repite para cada columna seleccionada,    garantizando así que el efecto de gravedad se aplique únicamente donde sea necesario antes de buscar nuevas fusiones.

    Finalmente, retorna:
    - Los índices donde cayeron bloques tras la aplicación de la gravedad
    - La grilla modificada

    Cabe destacar que este proceso fue diseñado para aplicar la gravedad solo donde es necesario. Si bien no representa una mejora significativa en rendimiento en el caso actual, permite escalar el juego a una versión con más columnas y filas sin comprometer la eficiencia.

---

- <span style="color:#fc7f40">📌**Aviso "Combo x N.**</span> 

    La manera en la que se evalúa si se mostrará el aviso de "Combo xN" está completamente manejada en el frontend. Para esto, se aprovecha la misma consulta de disparo que se utiliza para obtener el estado actualizado de la grilla y los efectos que produce la jugada.

    Cuando el jugador dispara un bloque, la respuesta de Prolog incluye una lista de efectos que detallan los cambios ocurridos durante esa jugada. Cada efecto tiene dos componentes:

    - La grilla resultante luego de ese paso.

    - Una lista que puede incluir, los nuevos bloques generados como resultado de fusiones (si es que los hay).

    Desde el frontend, no es necesario realizar consultas extra ni cálculos adicionales sobre la grilla. Lo que se hace es recorrer esta lista de efectos y contabilizar cuántos de esos efectos tienen una lista de nuevos bloques (newBlock) no vacía. Cada vez que esa lista contiene al menos un bloque, significa que se produjo una fusión en ese paso.

    Una vez realizada esta contabilización, si la cantidad de fusiones detectadas es mayor o igual a 3, se activa el aviso en pantalla "Combo xN", donde N corresponde a la cantidad total de fusiones encadenadas en esa jugada.

    Como en el juego original, si en un efecto se fusionan varios bloques distintos, esto aún se cuenta como un solo incremento en el combo. Esta elección intenta representar con precisión el comportamiento del juego base.

- <span style="color:#fc7f40">📌**Aviso de nuevo bloque máximo logrado**</span>

    Desde el front-end se lleva un seguimiento permanente del bloque máximo presente en la grilla. Inicialmente, este valor se establece en `0`. Si al cargar la grilla esta ya contiene bloques, se busca el valor máximo actual y se almacena como referencia.

    Cada vez que se genera un nuevo bloque mediante una fusión, se compara su valor con el máximo registrado. Si supera al anterior, se actualiza el valor almacenado. Sin embargo, las notificaciones visuales solo se activan cuando el nuevo bloque alcanza al menos el valor `512`.

    Cuando esta condición se cumple, el nuevo valor se guarda temporalmente en un estado reservado para notificaciones. Si no hay otras notificaciones activas, se despliega en pantalla un cartel tipo *pop-up* que anuncia el nuevo bloque máximo alcanzado. Luego de ser mostrado, el estado de notificación vuelve a su valor nulo, permitiendo futuras notificaciones.

- <span style="color:#fc7f40">📌**Aviso de nuevo bloque agregado al rango de tiro**</span>

    El seguimiento del bloque máximo también permite determinar el desbloqueo de nuevos bloques que se incorporan al rango de tiro. Cuando se supera un determinado umbral, se agrega un nuevo bloque al conjunto de posibles bloques disparables.

    Estos umbrales están definidos en una tabla incluida en el repositorio de GitHub. A medida que se alcanzan, los bloques correspondientes se desbloquean automáticamente.

    Una vez superado el valor `16384`, se abandona el uso de la tabla fija y los bloques a desbloquear se calculan utilizando el mismo criterio que el generador aleatorio de bloques para el rango de tiro. No obstante, en este caso solo se calcula el nuevo valor máximo a desbloquear, ya que es el único que debe notificarse.

- <span style="color:#fc7f40">📌**Aviso de nuevo bloque retirado**</span>

    Este mecanismo representa una combinación de lógica de back-end y front-end. Desde el back-end, al momento de ejecutar el disparo, se determina si se ha eliminado algún bloque durante las fusiones. Si esto ocurre, se identifica cuál fue el bloque de mayor valor eliminado y se incluye esta información en la respuesta de la consulta de disparo.

    El front-end, al recibir esta información, lo muestra al usuario mediante una notificación visual.

    > Cabe destacar que todas las notificaciones del juego se presentan de manera secuencial, nunca simultánea. Esto garantiza que no se superpongan en pantalla y permite apreciar correctamente las transiciones suaves entre cada una de ellas.

- <span style="color:#fc7f40">📌**Limpieza de bloques retirados**</span>

    Este procedimiento se realiza íntegramente desde el back-end. Luego de completar el ciclo de fusiones, se evalúa si corresponde eliminar algún bloque de la grilla, en función del valor máximo alcanzado tras dichas fusiones.

    A partir del bloque `1024` en adelante, se identifica automáticamente el bloque a retirar: este corresponde al bloque inmediatamente anterior al menor del rango de tiro, el cual se define como la mitad de dicho valor.

    Si se detecta un bloque que debe ser retirado, se procede a eliminar todas sus apariciones en la grilla. Durante este proceso, se registran las posiciones de los bloques eliminados con el fin de aplicar gravedad únicamente en las columnas afectadas.

    Una vez aplicada la gravedad, se calculan los efectos y las nuevas posiciones de los bloques desplazados, y se continúa con la ejecución de `process` para evaluar posibles nuevas fusiones y repeticiones del ciclo.

- <span style="color:#fc7f40">📌**Booster Hint jugada**</span>

    Esta funcionalidad se implementa mediante la cooperación entre el back-end y el front-end. 

    Por un lado, el back-end se encarga de simular, para cada columna de la grilla, un disparo con el fin de calcular tanto el combo que se lograría como el bloque máximo que se generaría como resultado de las fusiones. 

    Por otro lado, el front-end permite activar o desactivar esta funcionalidad desde la interfaz. Según su estado, el sistema decidirá si debe calcular y mostrar la información relacionada con los combos logrados y bloques generados o, en caso contrario, omitir este procesamiento.

- <span style="color:#fc7f40">📌**Booster Bloque siguiente**</span>

    El booster de bloque siguiente está implementado del lado del frontEnd. Al iniciarse la partida por primera vez, durante la inicialización del juego (init), se consultan dos bloques aleatorios: uno para ser el primero en disparar, y otro que queda en espera como siguiente bloque.

    Independientemente de si el booster está activado o no, esta lógica se ejecuta de todas formas. Es decir, luego de disparar el bloque actual, este se reemplaza directamente por el valor previamente asignado como "bloque siguiente", y recién entonces se realiza una nueva consulta para generar un nuevo bloque siguiente.

    En el caso de que el siguiente bloque a disparar corresponda a un bloque que fue retirado de la grilla en la misma jugada, este no debe tomar el lugar del bloque próximo a disparar. En su lugar, se descarta y se genera un nuevo bloque aleatorio mediante una consulta, teniendo en cuenta el estado actualizado de la grilla.

    El botón asociado al booster no altera esta mecánica. Su única función es alternar entre mostrar u ocultar el valor del próximo bloque, pero no modifica el hecho de que siempre se calcula por adelantado.

---

# 🔽 Puntos a destacar

- Se creo el concepto de "Game Over", es decir, al no tener mas espacios libres en la grilla se muestra un cartel de fin de juego que contabiliza los puntos y ofrece la opcion de reiniciar y volver a jugar.

- Se retoco el frontEnd con fin de estilizar el juego para que se adapte *mejor* a pantallas de distintos tamaños.

- Se estilizaron las notificaciones para que se parezcan a las del juego original.

- Se distribuyo eficientemente la carga de tareas entre el frontEnd y el backEnd, es decir, los calculos que se podian hacer desde el frontEnd no fueron consultados al backEnd.



---

# 🔽 Casos de prueba relevantes

        Aclaracion:
            -El rendimiento del juego no se ve reflejado en las grabaciones

## Caso fusiones multiples hacia arriba + remocion de multiples bloques en desuso + fusiones multiples

![Alt Text](/docs/casosDePrueba/casoDePrueba1.gif)

Si bien en este caso se muestran bloques que no debieran estar presentes a esa altura del juego (2,4,8), este caso *hardcodeado* ilustra la capacidad del sistema a resolver estas situaciones.

## Caso fusiones multiples en mismo ciclo no constituye combo

![Alt Text](/docs/casosDePrueba/casoDePrueba2.gif)

![Alt Text](/docs/casosDePrueba/casoDePrueba3.gif)

En este caso se puede apreciar el concepto de ciclo, ya que 6 bloques se fusionaron resultando en 3 pero para el conteo del combo, esto vale 1, tal como es en el juego original.

## Caso fusion sobre posicion distinta a la de caida

![Alt Text](/docs/casosDePrueba/casoDePrueba4.gif)

En este caso se puede ver que la fusion no se hace sobre la posicion donde cayo el bloque, sino que se encuentra un mejor candidato y la fusion culmina sobre el mismo tal como en el juego original.

## Caso fin de juego

![Alt Text](/docs/casosDePrueba/casoDePrueba5.gif)