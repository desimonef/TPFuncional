
# Sistema de Workflows en Haskell

Este proyecto implementa un motor de ejecución de workflows en Haskell, que permite definir tareas con dependencias, inputs y outputs, ejecutar scripts en contenedores Docker, y consultar resultados a través de una API RESTful.

## Características principales

- Definición de workflows en JSON
- Ejecución de tareas en Python, Bash, JavaScript, C
- Dependencias entre tareas y planificación topológica
- Manejo de reintentos y estrategias de fallo
- Interfaz REST completa para gestión y consulta
- Uso de contenedores Docker para aislamiento de ejecución
- Persistencia en base de datos SQLite
- Modelado funcional con separación entre código puro y efectos

## Requisitos

Para compilar y ejecutar el sistema, se requiere:

- [GHC](https://www.haskell.org/ghc/) (Glasgow Haskell Compiler), versión 9.x recomendada
- [Cabal](https://www.haskell.org/cabal/) (sistema de construcción de proyectos Haskell)
- [Docker](https://www.docker.com/) (para la ejecución de tareas en contenedores)
