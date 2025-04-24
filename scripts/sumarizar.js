const fs = require('fs');
const valores = fs.readFileSync(process.argv[2], 'utf-8').trim().split("\\n").map(Number);
const extra = process.argv[3];
console.log("Suma total:", valores.reduce((a, b) => a + b, 0), "| Extra:", extra);
