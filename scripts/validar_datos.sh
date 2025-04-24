#!/bin/bash
if grep -q "valor" "$1"; then
  echo "OK" > valido.txt
  echo "OK"
else
  echo "INVALIDO" > valido.txt
  echo "INVALIDO"
fi
