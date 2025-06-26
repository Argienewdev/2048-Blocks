# Informe de la Resolución del Proyecto
*M2 Blocks - Lógica para Cs. de la Computación - 2025*

**Comisión `46`**:
- `Salamanca, Santiago`
- `Santa María, Joaquín`
---
Dadas las principales funcionales a ser contempladas tal como estan presentadas en el repositorio de GitHub, a continuacion se describirá como se implementaron.


- <span style="color:#fc7f40">📌 **Generación aleatoria del bloque a disparar.**</span> 

    (Hacer)

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

    El núcleo de la lógica del juego se basa en tres niveles de predicados encadenados de forma jerárquica:

    #### 1. `admin` (nivel superior):

    Controla el ciclo completo del disparo de un bloque. Se encarga de:

    - Invocar a `bucle` para identificar fusiones iniciales  a partir del índice del nuevo bloque.
    - Aplicar la gravedad una vez que se realizaron fusiones.
    - Invocar nuevamente `bucle` para revisar nuevas posibles fusiones  que puedan haber surgido tras la caída de bloques y fusiones previas.

    > Como técnica de optimización, se utilizó la implicación (**`->`**) para evitar reevaluaciones innecesarias cuando aplicar gravedad no provoca ningun cambio.

    #### 2. `bucle` (nivel intermedio):

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

    #### 3. `fusion` (nivel inferior):

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
     
    Luego de completar todas las fusiones posibles, se procede a revisar si se generó un nuevo bloque máximo en la grilla y se eliminan los  bloques que ya no están en uso. Esto se detalla en la siguiente sección.

    ---

    ### 🔽 Aplicación de la gravedad

    Una vez concluidas las fusiones en una grilla, es posible que queden espacios vacíos entre bloques, debido a que algunas celdas fueron  liberadas tras las combinaciones. Para simular el efecto de la gravedad, se diseñó un mecanismo que reorganiza los bloques de cada columna, haciendo que los espacios vacíos se acumulen en la parte superior.

    Esto se logra recorriendo cada columna de la grilla, extrayendo sus elementos de forma individual. Luego, se reordena dicha columna de modo que todos los bloques no vacíos se agrupen al fondo, y los guiones (que representan espacios vacíos) queden por encima. Finalmente, se  reinserta la columna ya ordenada en su posición original dentro de la    grilla.

    Este procedimiento se repite para cada columna, garantizando así que el efecto de gravedad se aplique a toda la grilla antes de buscar nuevas fusiones.

    Finalmente, retorna:
    - Los índices donde cayeron bloques tras la aplicación de la gravedad
    - La grilla modificada

    Se consideró una optimización que consistía en limitar el recorrido únicamente a las columnas adyacentes a los bloques que se desplazaron. Sin embargo, esta alternativa fue descartada debido al drástico aumento en la complejidad del código, frente a una mejora en el rendimiento que resultó ser despreciable en la práctica.