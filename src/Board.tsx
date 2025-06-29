/*
 Board.tsx - Modificaciones para mostrar pistas visuales del Booster Hint
 Este código recibe una lista de "hints" y dibuja sobre cada columna una pista flotante,
 indicando el combo potencial que se lograría si se dispara el bloque actual en esa columna.
*/

import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
    grid: Grid;
    numOfColumns: number;
    onLaneClick: (lane: number) => void;
    // NUEVO: Lista de pistas a mostrar, una por cada columna
    hints?: { col: number, combo: number }[];
}

function Board({ grid, numOfColumns, onLaneClick, hints = [] }: BoardProps) {
    const numOfRows = grid.length / numOfColumns;

    return (
        <div className="board">
            <div className="blocks" style={{ gridTemplateColumns: `repeat(${numOfColumns}, 70px)`, gridTemplateRows: `repeat(${numOfRows}, 70px)` }}>

                {/* NUEVO: Dibujamos una pista sobre cada columna si existe un hint para esa columna */}
                {hints.map((hint, i) => (
                    <div
                        key={`hint-${i}`}
                        className="hint"
                        style={{
                            gridColumn: hint.col,
                            gridRow: 1,
                            position: 'relative',
                            textAlign: 'center',
                            opacity: 0.7,
                            color: 'white',
                            fontSize: '12px',
                            zIndex: 2
                        }}
                    >
                        {hint.combo >= 3 ? `Combo x${hint.combo}` : 'Sin combo'}
                    </div>
                ))}

                {/* Renderizamos las zonas clickeables de cada columna */}
                {Array.from({ length: numOfColumns }).map((_, i) => (
                    <div
                        className='lane'
                        style={{ gridColumn: i + 1, gridRow: `1 / span ${numOfRows}` }}
                        onClick={() => onLaneClick(i + 1)}
                        key={i}
                    />
                ))}

                {/* Renderizamos los bloques visibles de la grilla */}
                {grid.map((num, i) => {
                    if (num === "-") return null;
                    const pos: Position = [Math.floor(i / numOfColumns), i % numOfColumns];
                    return (
                        <Block
                            value={num}
                            position={pos}
                            key={i}
                        />
                    );
                })}
            </div>
        </div>
    );
}

export default Board;
