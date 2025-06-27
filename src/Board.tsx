import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
    grid: Grid;
    numOfColumns: number;
    onLaneClick: (lane: number) => void;
    // lista de pistas a mostrar, una por cada columna
    hints?: { col: number, combo: number }[];
}

function Board({ grid, numOfColumns, onLaneClick, hints = [] }: BoardProps) {
    const numOfRows = grid.length / numOfColumns;

    return (
        <div className="board">
            <div className="blocks" style={{ gridTemplateColumns: `repeat(${numOfColumns}, 70px)`, gridTemplateRows: `repeat(${numOfRows}, 70px)` }}>

                {/* se dibuja una pista sobre cada columna si existe un hint para esa columna */}
                {hints.map((hint, i) => (
                    <div
                        key={`hint-${i}`}
                        style={{ //TODO: Agregar esto al css, por algun motivo no reconoce la clase
                            position: 'relative',
                            textAlign: 'center',
                            opacity: 1,
                            color: 'white',
                            fontSize: '12px',
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
