import { cache, useEffect, useState, useTransition } from 'react';
import PengineClient, { PrologTerm } from './PengineClient';
import Board from './Board';
import Block from './Block';
import { delay } from './util';
import { resolveTypeReferenceDirective } from 'typescript';
import { parse } from 'path';

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
  // Server related states
  const [pengine, setPengine] = useState<any>(null);
  const [waiting, setWaiting] = useState<boolean>(false);

  //Screen width
  const [screenWidth, setScreenWidth] = useState<number>(window.innerWidth);

  //Game related
  const [grid, setGrid] = useState<Grid | null>(null);
  const [numOfColumns, setNumOfColumns] = useState<number | null>(null);
  const [score, setScore] = useState<number>(0);
  const [gameOver, setGameOver] = useState<boolean>(false);
  const [shootBlock, setShootBlock] = useState<number | null>(null);
  const [maxBlock, setMaxBlock] = useState<number>(0); // Valor máximo alcanzado
  const [maxRange, setMaxRange] = useState<number>(0);
  const [minRange, setMinRange] = useState<number>(0);
  const [shootAnimationKey, setShootAnimationKey] = useState(0);

  const [minBlockDeleted, setMinBlockDeleted] = useState<number | null>(null); // Cartel de bloque eliminado
  const [showRemovedBlock, setShowRemovedBlock] = useState<boolean>(false);

  
  const [comboNotification, setComboNotification] = useState<string | null>(null);
  const [showComboNotification, setShowComboNotification] = useState<boolean>(false);
  const [fadeShowComboNotification, setFadeShowComboNotification] = useState<boolean>(false);
  
  const [newBlockAdded, setNewBlockAdded] = useState<number | null>(null); // Cartel de nuevo bloque
  const [showNewBlockAdded, setShowNewBlockAdded] = useState<boolean>(false);
  const [fadeNewBlockAddedNotification, setFadeNewBlockAddedNotification] = useState<boolean>(false);
  const [showNewBlockAddedNotification, setShowNewBlockAddedNotification] = useState<boolean>(false);
  
  const [nextBlockVisible, setNextBlockVisible] = useState<boolean>(false);
  const [nextBlock, setNextBlock] = useState<number>(0);   // Estado que almacena el próximo bloque a utilizar si el modo está activado
  
  const [newMaxBlock, setNewMaxBlock] = useState<number | null>(null); // Cartel de nuevo máximo
  
  // Estado que indica si los hints están activados o no.
  // - Cuando el usuario presiona el botón "Hint Jugada", este estado se invierte (true ↔ false).
  // - Si está activado, se mostrarán los hints después de cada jugada.
  // - Si está desactivado, los hints se ocultan y no se vuelven a calcular.
  const [hints, setHints] = useState<{ col: number, combo: number , maxBlock: number}[]>([]);
  const [hintsEnabled, setHintsEnabled] = useState<boolean>(false);


  //Restart states for a quick reset
  const [restartGrid, setRestartGrid] = useState<Grid | null>(null);
  const [restartNumOfColumns, setRestartNumOfColumns] = useState<number | null>(null);
  const [restartShootBlock, setRestartShootBlock] = useState<number | null>(null);
  const [restartNextBlock, setRestartNextBlock] = useState<number>(0);
  const [restartMaxBlock, setRestartMaxBlock] = useState<number>(0);
  const [restartMaxRange, setRestartMaxRange] = useState<number>(0);
  const [restartMinRange, setRestartMinRange] = useState<number>(0);

  //Cache every possibility
  const [shootColumns, setShootColumns] = useState<any[]>([]);

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

  async function connectToPenginesServer() {
    setPengine(await PengineClient.create()); // Await until the server is initialized
  }

  async function initGame() {
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block1, Min, Max, GridMax), randomBlock(Grid, Block2, _, _, _)';
    const response = await pengine!.query(queryS);
    setGrid(response['Grid']);
    setShootBlock(response['Block1']);
    setShootAnimationKey(k => k + 1);
    setNumOfColumns(response['NumOfColumns']);
    setNextBlock(response['Block2']);
    setMinRange(response['Min']);
    setMaxRange(response['Max']);
    setMaxBlock(response['GridMax']);

    await cacheShoots(response['Grid'], response['Block1'], response['NumOfColumns']);
  }

  async function cacheShoots(grid: Grid, shootBlock: number, numOfColumns: number) {
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const query = `shootCache(${shootBlock}, ${gridS}, ${numOfColumns}, Cache)`;

    const response = await pengine!.query(query);

    if (response && response['Cache']) {
      const parsedCache = response['Cache'].map((cache: any) => ({
        block: cache.args[0],
        lane: cache.args[1],
        grid: cache.args[2],
        numCols: cache.args[3],
        effects: cache.args[4],
        maxRemovedBlock: cache.args[5],
        combo: cache.args[6],
        maxBlockGenerated: cache.args[7],
        rGrid: cache.args[8],
        randomBlock: cache.args[9],
        minRangeRandomBlock: cache.args[10],
        maxRangeRandomBlock: cache.args[11],
        gridMaxBlock: cache.args[12],
      }));
      setShootColumns(parsedCache);
    }
  }

  /**
   * Called when the player clicks on a lane.
   */
  async function handleLaneClick(lane: number) {

    if (waiting || gameOver || newMaxBlock !== null || minBlockDeleted !== null || (minBlockDeleted !== null && !showRemovedBlock)) {
      return;
    }

    setWaiting(true);
    const response = shootColumns[lane - 1];

    if (response) {
      verifyNextBlockCorrectness(response);

      const cachePromise = cacheShoots(response.rGrid, nextBlock, numOfColumns!);
      await animateEffect(response.effects);

      evaluateNewMax(response);
      evaluateCombo(response);
      
      await cachePromise;
    } else { // Si no hay respuesta válida, se reactiva la interfaz
      setWaiting(false);
    }
  }

  async function verifyNextBlockCorrectness(response: any){
    /*
    Se contempla el caso en el que el siguiente bloque fue eliminado
    en el tiro anterior a que este sea usado, por lo que se debe pedir
    un nuevo bloque aleatorio en base a la nueva grilla
    */
    const newBlockValue = response.block;
    const newRandomBlockLastGrid = response.rGrid;
    const maxRemovedBlock = response.maxRemovedBlock;
    if(maxRemovedBlock >= nextBlock){
      const parsedFinalGrid = JSON.stringify(newRandomBlockLastGrid).replace(/"/g, '');
      const newRandomBlockQuery = `randomBlock(${parsedFinalGrid}, Block, _, _, _)`;
      const newRandomBlockResponse = await pengine.query(newRandomBlockQuery);
      const newRandomBlock = newRandomBlockResponse['Block'];
      if(newRandomBlockResponse){
        setShootBlock(newRandomBlock)
        setShootAnimationKey(k => k + 1);
        setNextBlock(newBlockValue);
      }
    }else{
      setShootBlock(nextBlock);
      setShootAnimationKey(k => k + 1);
      setNextBlock(newBlockValue);
    }
  }

  function evaluateNewMax(response: any){
    const currentMax = response.gridMaxBlock;
      const removedBlock = response.maxRemovedBlock;
      const newMaxRange = response.maxRangeRandomBlock;

      if (currentMax > maxBlock) {
        setMaxBlock(currentMax);
        
        if(newMaxRange > maxRange!){
          setNewBlockAdded(newMaxRange);
        }

        if (currentMax >= 512) {
          setNewMaxBlock(currentMax);
        }
        
        if (!showRemovedBlock && removedBlock !== 0) {
          setMinBlockDeleted(removedBlock);
        }
      }
  }

  function evaluateCombo(response: any){
    // Cuento cuántos efectos contienen información de fusión
    // (cada efecto con args[1].length > 0 indica una fusión)
    const fusionCount = response.effects.filter((eff: EffectTerm) =>
      eff.args[1].length > 0
    ).length;
    // Después de completar todas las animaciones, mostramos notificación si la cantidad de fuciones es mayor igual a 3
    if (fusionCount >= 3) {
      setComboNotification(`¡Combo x${fusionCount}!`);
    }
  }

  /**
   * Displays each grid of the sequence as the current grid in 0.25sec intervals, 
   * and considers the other effect information.
   * @param effects The list of effects to be animated.
   */
  async function animateEffect(effects: EffectTerm[]): Promise<Grid> {
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
        prepareRestart();
      }

      return effectGrid; 
    }

    await delay(250);
    return await animateEffect(restRGrids);
  }

  //----------------------------------------------------------------------------------------------
  //Cuando se actualice la cache de shoots se actualizan las pistas por columna usando esta informacion
  useEffect(() => {
    if (shootColumns.length > 0 && !gameOver && !waiting) {
      handleHintInternal();
    }
  }, [shootColumns, waiting]);

  //----------------------------------------------------------------------------------------------

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

    // Programo la eliminación completa
    const removeTimeout = setTimeout(() => {
      setComboNotification(null);                     // Limpiar el mensaje
      setFadeShowComboNotification(false);            // Resetear estado de desvanecimiento
      setShowComboNotification(false);                // Ocultar completamente el elemento
    }, 1000);
    
    //cancelar timeouts si el componente finaliza
    return () => {
      clearTimeout(fadeTimeout);
      clearTimeout(removeTimeout);
    };
  }, [comboNotification]); // Dependencia: solo se ejecuta cuando comboNotification cambia
  
  //----------------------------------------------------------------------------------------------
  // Efecto para manejar la animación de notificaciones
  // Se activa cada vez que cambia el estado 'newBlockAdded'
  useEffect(() => {
    // Si no hay notificación, no hacer nada
    const shouldShow = newBlockAdded !== null && (showNewBlockAdded || (newMaxBlock == null && comboNotification == null && !showRemovedBlock));
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
  }, [newBlockAdded, showNewBlockAdded, newMaxBlock, comboNotification, showRemovedBlock]);
  
  //----------------------------------------------------------------------------------------------

  async function handleHintInternal() {
    const parsedHints = Array.from({ length: numOfColumns! }, (_, i) => {
      const response = shootColumns[i];

      return {
        col: response.lane,
        combo: response.combo,
        maxBlock: response.maxBlockGenerated,
      };
    });

    setHints(parsedHints);
  }

  //----------------------------------------------------------------------------------------------

  function isGridFull(grid: Grid): boolean {
    return !grid.includes("-");
  }

  //----------------------------------------------------------------------------------------------


  async function prepareRestart(){
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block1, MinRange, MaxRange, GridMax), randomBlock(Grid, Block2, _, _, _)';
    const response = await pengine!.query(queryS);
    const nextBlock = response['Block2'];
    
    setRestartGrid(response['Grid']);
    setRestartShootBlock(response['Block1']);
    setRestartNumOfColumns(response['NumOfColumns']);
    setRestartNextBlock(response['Block2']);
    setRestartMaxBlock(response['GridMax']);
    setRestartMaxRange(response['MaxRange']);
    setRestartMinRange(response['MinRange']);
    
    await cacheShoots(response['Grid'], response['Block1'], response['NumOfColumns']);

    const gridNumbers = response['Grid'].filter((v: any) => v !== '-').map(Number);
    const initialMax = gridNumbers.length > 0 ? Math.max(...gridNumbers) : 0;
    const maxBlock = initialMax;
    setMaxBlock(initialMax);
  }
  
  //----------------------------------------------------------------------------------------------

  async function restartGame() {
    setGameOver(false);
    setScore(0);
    setHints([]);
    setHintsEnabled(false);
    setShowRemovedBlock(false);
    setGrid(restartGrid);
    setShootBlock(restartShootBlock);
    setShootAnimationKey(k => k + 1);
    setNumOfColumns(restartNumOfColumns);
    setNextBlock(restartNextBlock);
    setMaxBlock(restartMaxBlock);
    setMaxRange(restartMaxRange);
    setMinRange(restartMinRange);
    handleHintInternal();
  }

  //----------------------------------------------------------------------------------------------

  if (grid === null) {
    return null;
  }

  return (
    <div className="game" style={{ position: 'relative' }}>
      {gameOver && (
        <div className="fullscreenOverlay">
          <div className="invasivePopUpCard">
            <h2>¡Game Over!</h2>
            <p>Puntaje: {score}</p>
            <button onClick={restartGame}>Reiniciar juego</button>
          </div>
        </div>
      )}

      {/*------- CARTEL NUEVO MAXIMO -------*/}
      {!comboNotification && newMaxBlock !== null && (
        <div className="fullscreenOverlay fadeIn">
          <div className="invasivePopUpCard">
            <h2>¡Nuevo máximo alcanzado!</h2>
            <p>Se alcanzó el bloque</p>
            <div className='invasivePopUpCardBlockContainerDiv'>
              {(<Block value={newMaxBlock!} position={[0, 0]} nonInvasivePopUp={false} />)}
            </div>
            <button onClick={() => {
              setNewMaxBlock(null);
              setShowRemovedBlock(true);
            }}>
              Aceptar
            </button>
          </div>
        </div>
      )}
      
      {/*------- CARTEL BLOQUE ELIMINADO -------*/}
      {minBlockDeleted !== null && showRemovedBlock && (
        <div className="fullscreenOverlay">
          <div className="invasivePopUpCard">
            <h2>¡Bloque eliminado!</h2>
            <p>El bloque {minBlockDeleted} fue eliminado de la grilla.</p>
            <div className='invasivePopUpCardBlockContainerDiv'>
              {(<Block value={minBlockDeleted!} position={[0, 0]} nonInvasivePopUp={false} />)}
            </div>
            <button onClick={() => {
              setMinBlockDeleted(null);
              setShowRemovedBlock(false);
              setShowNewBlockAdded(true);
            }}>
              Aceptar
            </button>
          </div>
        </div>
      )}
      {/*------- CARTEL BLOQUE AGREGADO AL RANGO DE TIRO -------*/}
      {newBlockAdded !== null &&
      (showNewBlockAdded || (newMaxBlock == null && !comboNotification)) && (
        <div className={`newBlockAddedTopDiv`}>
          <div className={`newBlockAddedCard ${showNewBlockAddedNotification ? 'show' : ''} ${fadeNewBlockAddedNotification ? 'fade-out' : ''}`}>
            <h2>¡Bloque </h2>
            <div className={`newBlockAddedBlockContainerDiv ${showNewBlockAddedNotification ? 'show' : ''} ${fadeNewBlockAddedNotification ? 'fade-out' : ''}`}>
              {(<Block value={newBlockAdded!} position={[0, 0]} nonInvasivePopUp={true} />)}
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
        hintsEnabled={hintsEnabled}
      />

      <div className="footer">
        <button className={`boosterHintJugada ${hintsEnabled ? 'visible' : ''}`} onClick={() => setHintsEnabled(!hintsEnabled)}>
          Hint Jugada
        </button>
        <div className= "blockShootContainerDiv">
          <div key={shootAnimationKey} className="blockShoot fade-in">
            <Block value={shootBlock!} position={[0, 0]} nonInvasivePopUp={false}/>
          </div>
        </div>
        <div className= "nextBlockContainerDiv">
          <button className={`boosterBloqueSiguiente ${nextBlockVisible ? 'visible' : ''}`} onClick={() => setNextBlockVisible(!nextBlockVisible)}>
            {!nextBlockVisible ? '?' : (<Block value={nextBlock!} position={[0, 0]} nonInvasivePopUp={false}/>)}
          </button>
        </div>
      </div>
    </div>
  );
}

export default Game;