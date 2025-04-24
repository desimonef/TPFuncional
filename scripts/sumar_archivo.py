import csv
import sys
import math

archivo = sys.argv[1]

with open(archivo, newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    valores = [int(row["valor"]) for row in reader]

resultado = math.prod(valores)
print(resultado)
