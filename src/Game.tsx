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

  // NUEVO: Estados para el sistema de notificaciones de combos
  // - notification: Almacena el mensaje a mostrar ("¡Combo x3!", etc.)
  // - fade: Controla la animación de desvanecimiento al final
  // - show: Controla la visibilidad inicial de la notificación
  const [notification, setNotification] = useState<string | null>(null);
  const [fade, setFade] = useState<boolean>(false);
  const [show, setShow] = useState<boolean>(false);
  const [hints, setHints] = useState<{ col: number, combo: number }[]>([]);

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

  // NUEVO: Efecto para manejar la animación de notificaciones
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

     // Programo la eliminación completa después de 1000ms
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
    const queryS = 'init(Grid, NumOfColumns), randomBlock(Grid, Block)';
    const response = await pengine!.query(queryS);
    setGrid(response['Grid']);
    setShootBlock(response['Block']);
    setNumOfColumns(response['NumOfColumns']);
  }

  /**
   * Called when the player clicks on a lane.
   */
  async function handleLaneClick(lane: number) {
  // No effect if waiting. 
  if (waiting) {
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

    setShootBlock(response['Block']);

    // Paso fusionCount a la función de animación
    //Ejecuto la animación de efectos y ESPERO que termine completamente
    await animateEffect(response['Effects'], fusionCount);
    
    
     // Después de completar todas las animaciones, mostramos notificación si la cantidad de fuciones es mayor igual a 3
    if (fusionCount >= 3) {
      setNotification(`¡Combo x${fusionCount}!`);
    }

    if (fusionCount == 3) {
      handleHintInternal(response['Block']);
    }
   

  } else { // Si no hay respuesta válida, se reactiva la interfaz
    setWaiting(false);
  }
  
}

 async function animateEffect(effects: EffectTerm[], fusionCount: number) {
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
    if (shootBlock !== null) {
    handleHintInternal(shootBlock);
    }
    return;
  }
  
  await delay(250);
  await animateEffect(restRGrids, fusionCount);

  handleHint();
}

async function handleHintInternal(blockValue: number) {
    if (!grid || !numOfColumns) return;
    const gridS = JSON.stringify(grid).replace(/"/g, '');
    const queryS = `booster_hint(${blockValue}, ${gridS}, ${numOfColumns}, Hints)`;
    const response = await pengine.query(queryS);
    if (response && response['Hints']) {
      const parsedHints = response['Hints'].map((hint: any) => ({
        col: hint.args[0],
        combo: hint.args[1]
      }));
      setHints(parsedHints);
    }
}

async function handleHint() {
    if (shootBlock !== null) {
      handleHintInternal(shootBlock);
    }
  }

  if (grid === null) {
    return null;
  }

  return (
    /* NUEVO: Sistema de notificaciones de combos */
    <div className="game" style={{ position: 'relative' }}>
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
      />

      <div className="footer">
        <button className="powerUp1" onClick={handleHint}>Hint Jugada</button>
        <div className="blockShoot">
          <Block value={shootBlock!} position={[0, 0]} />
        </div>
        <button className="powerUp2" onClick={() => alert('¡PowerUp 2!')}>Bloque siguiente</button>
      </div>
    </div>
  );
}

export default Game;