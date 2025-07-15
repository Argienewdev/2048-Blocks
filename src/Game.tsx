import { useEffect, useState } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';

export type Grid = (number | "-")[];
interface EffectTerm extends PrologTerm {
  functor: "effect";
  args: [Grid, EffectInfoTerm[]];
}

type EffectInfoTerm = NewBlockTerm | PrologTerm;
interface NewBlockTerm extends PrologTerm {
  functor: "newBlock";
  args: [number];
}

function Game() {
  // State
  const [pengine, setPengine] = useState<any>(null);
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [screenWidth, setScreenWidth] = useState<number>(window.innerWidth);
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [gameOver, setGameOver] = useState<boolean>(false);
  const [showRemovedBlock, setShowRemovedBlock] = useState<boolean>(false);
  const [showNewBlockAdded, setShowNewBlockAdded] = useState<boolean>(false);

  // Estados para el sistema de notificaciones de combos
  // - comboNotification: Almacena el mensaje a mostrar ("¡Combo x3!", etc.)
  // - fade: Controla la animación de desvanecimiento al final
  // - show: Controla la visibilidad inicial de la notificación
  const [comboNotification, setComboNotification] = useState<string | null>(null);
  const [fadeShowComboNotification, setFadeShowComboNotification] = useState<boolean>(false);
  const [fadeNewBlockAddedNotification, setFadeNewBlockAddedNotification] = useState<boolean>(false);
  const [showComboNotification, setShowComboNotification] = useState<boolean>(false);
  const [showNewBlockAddedNotification, setShowNewBlockAddedNotification] = useState<boolean>(false);
  const [hints, setHints] = useState<{ col: number, combo: number }[]>([]);
  // Estado que indica si los hints están activados o no.
  // - Cuando el usuario presiona el botón "Hint Jugada", este estado se invierte (true ↔ false).
  // - Si está activado, se mostrarán los hints después de cada jugada.
  // - Si está desactivado, los hints se ocultan y no se vuelven a calcular.
  const [hintsEnabled, setHintsEnabled] = useState<boolean>(false);
  const [nextBlockVisible, setNextBlockVisible] = useState<boolean>(false);
  // Estado que almacena el próximo bloque a utilizar si el modo está activado
  const [nextBlock, setNextBlock] = useState<number>(0);

  //------- NUEVOS ESTADOS AGREGADOS --------
  const [maxBlock, setMaxBlock] = useState<number>(0); // Valor máximo alcanzado
  const [newMaxBlock, setNewMaxBlock] = useState<number | null>(null); // Cartel de nuevo máximo
  const [newBlockAdded, setNewBlockAdded] = useState<number | null>(null); // Cartel de nuevo bloque
  const [minBlockDeleted, setMinBlockDeleted] = useState<number | null>(null); // Cartel de bloque eliminado
  //-----------------------------------------

  useEffect(() => {
    // This is executed just once, after the first render.
    connectToPenginesServer();
  }, []);

  useEffect(() => {
    if (pengine) {
      // This is executed after pengine was set.
      initGame();
    }
  }, [pengine]);

  // Efecto para manejar la animación de notificaciones
  // Se activa cada vez que cambia el estado 'comboNotification'
  useEffect(() => {
    // Si no hay notificación, no hacer nada
    if (!comboNotification) return;

    // Muestro la notificación inmediatamente
    setShowComboNotification(true);
    setFadeShowComboNotification(false);

    // Programo el inicio del desvanecimiento después de 500ms
    const fadeTimeout = setTimeout(() => {
      setFadeShowComboNotification(true);              // Activar clase CSS para desvanecer
    }, 500);

    // Programo la eliminación completa después de 1s
    const removeTimeout = setTimeout(() => {
      setComboNotification(null); // Limpiar el mensaje
      setFadeShowComboNotification(false);             // Resetear estado de desvanecimiento
      setShowComboNotification(false);             // Ocultar completamente el elemento
    }, 1000);

    //cancelar timeouts si el componente finaliza
    return () => {
      clearTimeout(fadeTimeout);
      clearTimeout(removeTimeout);
    };
  }, [comboNotification]); // Dependencia: solo se ejecuta cuando comboNotification cambia

  // Efecto para manejar la animación de notificaciones
  // Se activa cada vez que cambia el estado 'newBlockAdded'
  useEffect(() => {
    // Si no hay notificación, no hacer nada
    const shouldShow = newBlockAdded !== null && (showNewBlockAdded || (newMaxBlock == null && !comboNotification));
    if (!shouldShow) return;

    // Muestro la notificación inmediatamente
    setShowNewBlockAddedNotification(true);
    setFadeNewBlockAddedNotification(false);

    // Programo el inicio del desvanecimiento después de 500ms
    const fadeTimeout = setTimeout(() => {
      setFadeNewBlockAddedNotification(true);              // Activar clase CSS para desvanecer
    }, 1500);

    // Programo la eliminación completa después de 1s
    const removeTimeout = setTimeout(() => {
      setNewBlockAdded(null);                          // Limpiar el mensaje
      setFadeNewBlockAddedNotification(false);             // Resetear estado de desvanecimiento
      setShowNewBlockAdded(false);                     // Ocultar completamente el elemento
    }, 2000);

    //cancelar timeouts si el componente finaliza
    return () => {
      clearTimeout(fadeTimeout);
      clearTimeout(removeTimeout);
    };
  }, [newBlockAdded, showNewBlockAdded, newMaxBlock, comboNotification]); // Dependencia: solo se ejecuta cuando comboNotification cambia

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create()); // Await until the server is initialized
  }

  async function initGame() {
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block1), randomBlock(Grid, Block2)';
    const response = await pengine!.query(queryS);
    setGrid(response['Grid']);
    setShootBlock(response['Block1']);
    setNumOfColumns(response['NumOfColumns']);
    setNextBlock(response['Block2']);

    //----------- Initialize grid max ------------
    const gridNumbers = response['Grid'].filter((v: any) => v !== '-').map(Number);
    const initialMax = gridNumbers.length > 0 ? Math.max(...gridNumbers) : 0;
    setMaxBlock(initialMax);

  }

  /**
   * Called when the player clicks on a lane.
   */
  async function handleLaneClick(lane: number) {
    // No effect if waiting. 
    if (waiting || gameOver || newMaxBlock !== null || minBlockDeleted !== null || (minBlockDeleted !== null && !showRemovedBlock)) {
      return;
    }
    /*
    Build Prolog query, which will be something like: 
    shoot(2, 2, [4,2,8,64,32,2,-,-,4,16,-,-,-,-,2,-,-,-,-,16,-,-,-,-,2,-,-,-,-,-,-,-,-,-,-], 5, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block).
    */
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects, MaxRemovedBlock), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);
    const response = await pengine.query(queryS);

    if (response) {
      // Cuento cuántos efectos contienen información de fusión
      // (cada efecto con args[1].length > 0 indica una fusión)
      const fusionCount = response['Effects'].filter((eff: EffectTerm) =>
        eff.args[1].length > 0
      ).length;
      

      //Se contempla el caso en el que el siguiente bloque fue eliminado
      //en el tiro anterior a que este sea usado, por lo que se debe pedir
      //un nuevo bloque aleatorio en base a la nueva grilla
      const newBlockValue = response['Block'];
      const newRandomBlockLastGrid = response['RGrid'];
      if(response['MaxRemovedBlock'] >= nextBlock){
        const parsedFinalGrid = JSON.stringify(newRandomBlockLastGrid).replace(/"/g, '');
        const newRandomBlockQuery = `randomBlock(${parsedFinalGrid}, Block)`;
        setWaiting(true);
        const newRandomBlockResponse = await pengine.query(newRandomBlockQuery);
        const newRandomBlock = newRandomBlockResponse['Block'];
        if(newRandomBlockResponse){
          setShootBlock(newRandomBlock)
          setNextBlock(newBlockValue);
          setWaiting(false);
        }else{
          setWaiting(false);
        }
      }else{
        setShootBlock(nextBlock);
        setNextBlock(newBlockValue);
      }

      // Paso fusionCount a la función de animación
      // Ejecuto la animación de efectos y ESPERO que termine completamente
      const finalGrid = await animateEffect(response['Effects'], fusionCount);

      //---------- Evaluate new max ------------
      const currentMax = Math.max(...(finalGrid.filter((v): v is number => v !== '-') ));
      const removedBlock = response['MaxRemovedBlock'];
      if (currentMax > maxBlock) {
        setMaxBlock(currentMax);
        
        let newBlockAdded = 0;
        if (currentMax >= 16) newBlockAdded = 8;
        if (currentMax >= 32) newBlockAdded = 16;
        if (currentMax >= 64) newBlockAdded = 32;
        if (currentMax >= 128) newBlockAdded = 64;
        if (currentMax >= 1024) newBlockAdded = 128;
        if (currentMax >= 2048) newBlockAdded = 256;
        if (currentMax >= 4096) newBlockAdded = 512;
        if (currentMax >= 16384) newBlockAdded = 1024;
        if (currentMax > 16384) {
          // A partir de 16k, se agrega el doble del maximo anterior
          // Calculo cuántas veces se duplicó después de 16k
          const duplicaciones = Math.floor(Math.log2(currentMax / 16384));
          newBlockAdded = 1024 * Math.pow(2, duplicaciones);
        }
        if(newBlockAdded !== 0){
          setNewBlockAdded(newBlockAdded);
        }

        if (currentMax >= 512) {
          setNewMaxBlock(currentMax);
        }
        
        if (!showRemovedBlock && removedBlock !== 0) {
          setMinBlockDeleted(removedBlock);
        }
      }
      // Después de completar todas las animaciones, mostramos notificación si la cantidad de fuciones es mayor igual a 3
      if (fusionCount >= 3) {
        setComboNotification(`¡Combo x${fusionCount}!`);
      }

      if (hintsEnabled) {
        // Solo se actualizan los hints automaticamente si el usuario activo el sistema de hints.
        // Se llama a handleHintInternal pasando la grilla final y el nuevo bloque para calcular los nuevos combos, sino se hace esto, se actualiza con bloque nuevo y grilla vieja.
        if (nextBlock!=null)
          await handleHintInternal(nextBlock, finalGrid);
      }

    } else { // Si no hay respuesta válida, se reactiva la interfaz
      setWaiting(false);
    }
  }

  /**
   * Displays each grid of the sequence as the current grid in 0.25sec intervals, 
   * and considers the other effect information.
   * @param effects The list of effects to be animated.
   */
  async function animateEffect(effects: EffectTerm[], fusionCount: number): Promise<Grid> {
    const effect = effects[0];
    const [effectGrid, effectInfo] = effect.args;
    setGrid(effectGrid);

    effectInfo.forEach((effectInfoItem) => {
      const { functor, args } = effectInfoItem;
      switch (functor) {
        case 'newBlock':
          setScore(score => score + args[0]);
          break;
        default:
          break;
      }
    });

    const restRGrids = effects.slice(1);
    if (restRGrids.length === 0) {
      setWaiting(false);
      if (isGridFull(effectGrid)) {
        setGameOver(true);
      }

      return effectGrid; 
      //Se devuelve la última grilla luego de completar 
      //todas las animaciones lo cual permite usarla en handleHintInternal 
      //para calcular los combos correctos y para evaluar el nuevo maximo.
    }

    await delay(250);
    return await animateEffect(restRGrids, fusionCount);
  }

  async function handleHintInternal(blockValue: number, currentGrid?: Grid, forzar = false) {
    if (!hintsEnabled && !forzar) return; //si no esta habilitado y no se forzo 

    // Si se paso una grilla explicitamente (desp de una jugada), se usa esa.
    // De lo contrario, se usa la grilla actual almacenada en el estado.
    const actualGrid = currentGrid || grid;

    //Verifica que tanto la grilla como el numero de columnas estén definidos.
    // Si falta alguno, no tiene sentido consultar a Prolog, así que se corta.(esto lo agregue porque me tiraba error)
    if (!actualGrid || !numOfColumns) return;

    //Serializa la grilla en formato compatible con Prolog (Lo saque del handleclick).
    const gridS = JSON.stringify(actualGrid).replace(/"/g, '');
    //Consulta a prolog booster_hint
    const queryS = `booster_hint(${blockValue}, ${gridS}, ${numOfColumns}, Hints)`;

    //Ejecuta la consulta en Prolog y espera la respuesta.
    const response = await pengine.query(queryS);

    //Si hay respuesta válida y contiene hints, se parsean.
    if (response && response['Hints']) {
      const parsedHints = response['Hints'].map((hint: any) => ({
        col: hint.args[0], // COLUMNA
        combo: hint.args[1], // CANTIDAD X DEL COMBO
        maxBlock: hint.args[2] 
      }));

      // Si no hay ninguna jugada sugerida, se limpia el estado de hints si no hay jugadas sugeridas.
      if (parsedHints.length > 0) {
        setHints(parsedHints);
      } else {
        setHints([]);
      }
    } else {
      //se limpia para que no queden sugerencias antiguas visibles
      setHints([]);
    }
  }

  async function handleHint() {
    if (hintsEnabled) {
      setHintsEnabled(false);
      setHints([]);
    } else {
      setHintsEnabled(true);

      if (shootBlock !== null) {
        await handleHintInternal(shootBlock, undefined, true);
      }
    }
  }

  function isGridFull(grid: Grid): boolean {
    return !grid.includes("-");
  }

  async function restartGame() {
    setGameOver(false);
    setScore(0);
    setHints([]);
    setHintsEnabled(false);
    setShowRemovedBlock(false);
    await initGame();
  }

  if (grid === null) {
    return null;
  }

  return (
    <div className="game" style={{ position: 'relative' }}>
      {gameOver && (
        <div className="game-over-overlay">
          <div className="game-over-card">
            <h2>¡Game Over!</h2>
            <p>Puntaje: {score}</p>
            <button onClick={restartGame}>Reiniciar juego</button>
          </div>
        </div>
      )}

      {/*------- CARTEL NUEVO MAXIMO -------*/}
      {!comboNotification && newMaxBlock !== null && (
        <div className="newMaxBlockOverlay">
          <div className="newMaxBlockCard">
            <h2>¡Nuevo máximo alcanzado!</h2>
            <p>Se alcanzó el bloque</p>
            <div className='newMaxBlockBlockContainerDiv'>
              {(<Block value={newMaxBlock!} position={[0, 0]} />)}
            </div>
            <button onClick={() => {
              setNewMaxBlock(null);
              setShowNewBlockAdded(true);
            }}>
              Aceptar
            </button>
          </div>
        </div>
      )}
      {/*------- CARTEL BLOQUE ELIMINADO -------*/}
      {minBlockDeleted !== null && showRemovedBlock && (
        <div className="removedBlockOverlay">
          <div className="removedBlockCard">
            <h2>¡Bloque eliminado!</h2>
            <p>El bloque {minBlockDeleted} fue eliminado de la grilla.</p>
            <div className='removedBlockBlockContainerDiv'>
              {(<Block value={minBlockDeleted!} position={[0, 0]} />)}
            </div>
            <button onClick={() => {
              setMinBlockDeleted(null);
              setShowRemovedBlock(false)
            }}>
              Aceptar
            </button>
          </div>
        </div>
      )}
      {/*------- CARTEL BLOQUE AGREGADO AL RANGO DE TIRO -------*/}
      {newBlockAdded !== null &&
      (showNewBlockAdded || (newMaxBlock == null && !comboNotification)) && (
        <div className={`newBlockAddedOverlay`}>
          <div className={`newBlockAddedCard ${showNewBlockAddedNotification ? 'show' : ''} ${fadeNewBlockAddedNotification ? 'fade-out' : ''}`}>
            <h2>¡Bloque </h2>
            <div className={`newBlockAddedBlockContainerDiv ${showNewBlockAddedNotification ? 'show' : ''} ${fadeNewBlockAddedNotification ? 'fade-out' : ''}`}>
              {(<Block value={newBlockAdded!} position={[0, 0]} />)}
            </div>
            <h2> agregado al rango de tiro!</h2>
          </div>
        </div>
      )}
      
      {/*------- NOTIFICACIONES DE COMBOS -------*/}
      {comboNotification && (
        <div
          // - 'show' para aparición inicial
          // - 'fade-out' para desvanecimiento
          className={`comboNotification ${showComboNotification ? 'show' : ''} ${fadeShowComboNotification ? 'fade-out' : ''}`}
        >
          {comboNotification}
        </div>
      )}

      <div className="header">
        <div className="score">{score}</div>
      </div>

      <Board
        grid={grid}
        numOfColumns={numOfColumns!}
        onLaneClick={handleLaneClick}
        hints={hints}
        shootBlock={shootBlock}
        screenWidth={screenWidth}
      />

      <div className="footer">
        <button className={`boosterHintJugada ${hintsEnabled ? 'visible' : ''}`} onClick={handleHint}>
          Hint Jugada
        </button>
        <div className="blockShoot">
          <Block value={shootBlock!} position={[0, 0]} />
        </div>
        <button className={`boosterBloqueSiguiente ${nextBlockVisible ? 'visible' : ''}`} onClick={() => setNextBlockVisible(!nextBlockVisible)}>
          {!nextBlockVisible ? '?' : (<Block value={nextBlock!} position={[0, 0]} />)}
        </button>
      </div>
    </div>
  );
}

export default Game;
