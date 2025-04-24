const fs = require('fs');
const contenido = fs.readFileSync('dato.txt', 'utf8');
const numero = parseInt(contenido.trim(), 10);
console.log(numero * 2);
