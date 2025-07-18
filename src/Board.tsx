import Block, { Position } from './Block';
import { Grid } from './Game';

interface BoardProps {
    grid: Grid;
    numOfColumns: number;
    onLaneClick: (lane: number) => void;
    hints: { col: number; combo: number; maxBlock: number }[];
    shootBlock: number | null;
    screenWidth: number;
    hintsEnabled: boolean;
}

function Board({ grid, numOfColumns, onLaneClick, hints, shootBlock, screenWidth, hintsEnabled }: BoardProps) {
    const numOfRows = grid.length / numOfColumns;
    const screenLaneWidth = screenWidth / 3;
    const maxLaneWidth = 70;
    const laneWidth = screenLaneWidth <= maxLaneWidth ? screenLaneWidth : maxLaneWidth;

    return (
        <div className="board">
            <div className="blocks" style={{ gridTemplateColumns: `repeat(${numOfColumns}, ${laneWidth}px)`, gridTemplateRows: `repeat(${numOfRows}, ${laneWidth}px)` }}>

            {/* Renderizamos los hints de cada columna */}
            {Array.from({ length: numOfColumns }).map((_, i) => {
                const hint = hints.find(h => h.col === i + 1); // Busca el hint para la columna i+1
                return (
                    <div className={`hints-container ${hintsEnabled ? 'visible' : ''}`} key={`hint-${i}`}>
                        {hint && (
                            <>
                            {hint.combo >= 3 ? `Combo x${hint.combo}` : 'Sin combo'}
                            <br/>
                            {hint.maxBlock && hint.maxBlock > 0 ? `Máx: ${hint.maxBlock}` : ''}
                            </>
                        )}
                    </div>
                );
                })}

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
                        nonInvasivePopUp={false}
                    />
                );
            })}
            </div>
        </div>
    );
}

export default Board;
