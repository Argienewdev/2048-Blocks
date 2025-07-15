export function numberToColor(num: number) {
    if (num === 0) return "black";
    const palette = [
        "#0083bb",
        "#fc7f40", 
        "#e6538a", 
        "#058555", 
        "#b82bfa", 
        "#a24e78", 
        "#b9c508", 
        "#1e90ff", 
        "#ff9800",
        "#e040fb", 
        "#00bfae", 
        "#ffd600",
        "#ff4081", 
        "#7c4dff", 
        "#00c853", 
        "#ff1744", 
        "#c51162", 
    ];
    // Calcula el exponente de 2 (por ejemplo, 2048 -> 11)
    const exp = Math.log2(num);
    // Redondea hacia abajo y ajusta el índice (2^1 = 2 debe ser index 0)
    const index = Math.max(0, Math.floor(exp) - 1);
    return palette[index % palette.length];
}

export function delay(milliseconds: number) {
    return new Promise(resolve => setTimeout(resolve, milliseconds));
}

/**
Formatea un número grande a una notación compacta:
Para números >= 16,384 muestra en formato '16k', '32k', etc.
Para millones muestra '1M', '2M', etc.
Para billones muestra '1B', '2B', etc.
*/
export function parseBigNumber(num: number): string {
    // Billones
    if (num >= 1_000_000_000) {
        // Redondea hacia abajo a la unidad de billón
        return Math.floor(num / 1_000_000_000) + 'B';
    }
    // Millones
    if (num >= 1_000_000) {
        // Redondea hacia abajo a la unidad de millón
        return Math.floor(num / 1_000_000) + 'M';
    }
    // Miles grandes (a partir de 16k)
    if (num >= 16_000) {
        // Redondea hacia abajo a la unidad de mil
        return Math.floor(num / 1_000) + 'K';
    }
    // Para el resto, muestra el número tal cual
    return num.toString();
}