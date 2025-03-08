const fs = require('fs');
const path = require('path');

const inputFile = "data.csv"; // Archivo de entrada
const outputFile = "data_output.csv"; // Archivo de salida

fs.readFile(inputFile, 'utf8', (err, data) => {
    if (err) {
        console.error("Error al leer el archivo:", err);
        process.exit(1);
    }
    
    // Convertir a mayúsculas
    const transformedData = data.toUpperCase();

    fs.writeFile(outputFile, transformedData, 'utf8', (err) => {
        if (err) {
            console.error("Error al escribir el archivo:", err);
            process.exit(1);
        }
        console.log(`Archivo procesado y guardado como ${outputFile}`);
    });
});
