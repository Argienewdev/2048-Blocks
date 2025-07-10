### Nota: 105

### ⚙️ Funcionalidad

#### 📌 Generación aleatoria del bloque a disparar
- ✅ funciona correctamente.
- ➕ se calcula el rango en base al máximo usando la tabla, hasta el 16k, y a partir de ahí se generaliza usando una fórmula.

#### 📌 Efecto del disparo de un bloque
- ✅ funciona correctamente.
- ➕ se consideró en la implementación el concepto de "ciclo" para garantizar el mismo comportamiento que el juego original, por ejemplo, al
contabilizar los combos, haciendo que fusines en simultáneo de diferentes grupos suman solo 1 al combo, y también para preservar fusiones separadas
cuando deben resolverse en simultáneo (pertenecen a un mismo ciclo), y no encadenarse. 

#### 📌 Avisos “Combo x N”
- ✅ funciona correctamente.

#### 📌 Avisos de nuevo bloque máximo logrado.
- ✅ funciona correctamente.

#### 📌 Limpieza de bloques retirados.
- ✅ funciona correctamente.

#### 📌 Booster Hint jugada
- ✅ funciona correctamente.

#### 📌 Booster Bloque siguiente.
- ✅ funciona correctamente.
➕ se contempla el caso en que el bloque siguiente quede fuera de rango luego del disparo del bloque actual, pidiendo un nuevo bloque aleatorio que respete el nuevo rango.

#### 🚀 Extras
- ➕ fin del juego

### 📚 Documentación
- ➕ La documentación en muy completa, y se encuentra muy bien organizada. Se explican los aspectos y decisiones más importantes de la implementación de los requerimientos.
- ✅ / ➕ Se incluyen casos de test en forma de gifs animados mostrando jugadas significativas.