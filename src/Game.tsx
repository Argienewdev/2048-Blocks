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
  const [score, setScore] = useState<number>(0);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [waiting, setWaiting] = useState<boolean>(false);
  const [gameOver, setGameOver] = useState<boolean>(false);

  // Estados para el sistema de notificaciones de combos
  // - notification: Almacena el mensaje a mostrar ("¡Combo x3!", etc.)
  // - fade: Controla la animación de desvanecimiento al final
  // - show: Controla la visibilidad inicial de la notificación
  const [notification, setNotification] = useState<string | null>(null);
  const [fade, setFade] = useState<boolean>(false);
  const [show, setShow] = useState<boolean>(false);
  const [hints, setHints] = useState<{ col: number, combo: number }[]>([]);
  // Estado que indica si los hints están activados o no.
  // - Cuando el usuario presiona el botón "Hint Jugada", este estado se invierte (true ↔ false).
  // - Si está activado, se mostrarán los hints después de cada jugada.
  // - Si está desactivado, los hints se ocultan y no se vuelven a calcular.
  const [hintsEnabled, setHintsEnabled] = useState<boolean>(false);
  const [nextBlockVisible, setNextBlockVisible] = useState<boolean>(false);
  // Estado que almacena el próximo bloque a utilizar si el modo está activado
  const [nextBlock, setNextBlock] = useState<number | null>(null);

  //------- NUEVOS ESTADOS AGREGADOS --------
const [maxBlock, setMaxBlock] = useState<number>(0); // Valor máximo alcanzado
const [newMaxBlock, setNewMaxBlock] = useState<number | null>(null); // Cartel de nuevo máximo
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
  // Se activa cada vez que cambia el estado 'notification'
  useEffect(() => {
    // Si no hay notificación, no hacer nada
    if (!notification) return;

    // Muestro la notificación inmediatamente
    setShow(true);
    setFade(false);

    // Programo el inicio del desvanecimiento después de 500ms
    const fadeTimeout = setTimeout(() => {
      setFade(true); // Activar clase CSS para desvanecer
    }, 500);

    // Programo la eliminación completa después de 1s
    const removeTimeout = setTimeout(() => {
      setNotification(null);  // Limpiar el mensaje
      setFade(false);         // Resetear estado de desvanecimiento
      setShow(false);         // Ocultar completamente el elemento
    }, 1000);

    //cancelar timeouts si el componente finaliza
    return () => {
      clearTimeout(fadeTimeout);
      clearTimeout(removeTimeout);
    };
  }, [notification]); // Dependencia: solo se ejecuta cuando notification cambia

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

    //------- INICIALIZAR MAXIMO INICIAL --------
    const initialMax = Math.max(...response['Grid'].filter((v: any) => v !== '-'));
    setMaxBlock(initialMax);
    //--------------------------------------------
  }

  /**
   * Called when the player clicks on a lane.
   */
  async function handleLaneClick(lane: number) {
    // No effect if waiting. 
    if (waiting || gameOver || newMaxBlock !== null || minBlockDeleted !== null) {
    return;
    }
    /*
    Build Prolog query, which will be something like: 
    shoot(2, 2, [4,2,8,64,32,2,-,-,4,16,-,-,-,-,2,-,-,-,-,16,-,-,-,-,2,-,-,-,-,-,-,-,-,-,-], 5, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block).
    */
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `shoot(${shootBlock}, ${lane}, ${gridS}, ${numOfColumns}, Effects), last(Effects, effect(RGrid,_)), randomBlock(RGrid, Block)`;
    setWaiting(true);
    const response = await pengine.query(queryS);

    if (response) {
      // Cuento cuántos efectos contienen información de fusión
      // (cada efecto con args[1].length > 0 indica una fusión)
      const fusionCount = response['Effects'].filter((eff: EffectTerm) =>
        eff.args[1].length > 0
      ).length;
      
      const newBlockValue = response['Block'];
      setShootBlock(nextBlock);
      setNextBlock(newBlockValue);

      // Paso fusionCount a la función de animación
      //Ejecuto la animación de efectos y ESPERO que termine completamente
      const finalGrid = await animateEffect(response['Effects'], fusionCount);

      // Después de completar todas las animaciones, mostramos notificación si la cantidad de fuciones es mayor igual a 3
      if (fusionCount >= 3) {
        setNotification(`¡Combo x${fusionCount}!`);
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

     //------- EVALUAR NUEVO MAXIMO --------
  const currentMax = Math.max(...(effectGrid.filter((v): v is number => v !== '-') ));

  if (currentMax > maxBlock) {
    if (currentMax >= 512) {
      setNewMaxBlock(currentMax); 
    }

    if (currentMax >= 1024) {
      let minDeleted = 2;

      if (currentMax >= 2048) minDeleted = 4;
      if (currentMax >= 4096) minDeleted = 8;
      if (currentMax >= 16384) {
        // A partir de 16k, se elimina el doble del eliminado anterior
        // Calculo cuántas veces se duplicó después de 16k
        const duplicaciones = Math.floor(Math.log2(currentMax / 16384));
        minDeleted = 16 * Math.pow(2, duplicaciones);
      }

      setMinBlockDeleted(minDeleted);
    }

    setMaxBlock(currentMax);
  }

  //-------------------------------------  

      return effectGrid; //Se devuelve la última grilla luego de completar todas las animaciones lo cual permite usarla en handleHintInternal 
      //para calcular los combos correctos. 
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

      {/*------- CARTEL BLOQUE ELIMINADO -------*/}
      {minBlockDeleted !== null && (
        <div className="game-over-overlay">
          <div className="game-over-card">
            <h2>¡Bloque eliminado!</h2>
            <p>El bloque {minBlockDeleted} fue eliminado de la grilla.</p>
            <button onClick={() => setMinBlockDeleted(null)}>Aceptar</button>
          </div>
        </div>
      )}
      {/*--------------------------------------*/}
      {/*------- CARTEL NUEVO MAXIMO -------*/}
      {newMaxBlock !== null && (
        <div className="game-over-overlay">
          <div className="game-over-card">
            <h2>¡Nuevo máximo alcanzado!</h2>
            <p>Se alcanzó el bloque {newMaxBlock}</p>
            <button onClick={() => setNewMaxBlock(null)}>Aceptar</button>
          </div>
        </div>
      )}
      {/*-----------------------------------*/}
      {/* notificaciones de combos */}
      {notification && (
        <div
          // - 'show' para aparición inicial
          // - 'fade-out' para desvanecimiento
          className={`combo-notification ${show ? 'show' : ''} ${fade ? 'fade-out' : ''}`}
        >
          {notification}
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
      />

      <div className="footer">
        <button className={`boosterHintJugada ${hintsEnabled ? 'visible' : ''}`} onClick={handleHint}>Hint Jugada</button>

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
