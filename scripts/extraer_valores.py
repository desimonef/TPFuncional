import sys
lines = open(sys.argv[1]).readlines()[1:]  # salta header
with open("valores.txt", "w") as f:
    for l in lines:
        f.write(l.split(",")[1].strip() + "\n")
print("valores.txt creado")
