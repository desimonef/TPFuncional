import sys
import collections

def analizar_texto(filename):
    try:
        with open(filename, 'r', encoding='utf-8') as file:
            text = file.read()
        
        palabras = text.split()
        num_palabras = len(palabras)
        num_caracteres = len(text)

        # Contar la frecuencia de palabras
        counter = collections.Counter(palabras)
        palabra_mas_frecuente, frecuencia = counter.most_common(1)[0]

        print(f"Análisis del archivo {filename}:")
        print(f"Número de palabras: {num_palabras}")
        print(f"Número de caracteres: {num_caracteres}")
        print(f"Palabra más frecuente: '{palabra_mas_frecuente}' (Aparece {frecuencia} veces)")
    
    except Exception as e:
        print(f"Error en el análisis: {e}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Uso: python analizar_texto.py <archivo>")
        sys.exit(1)
    
    analizar_texto(sys.argv[1])
