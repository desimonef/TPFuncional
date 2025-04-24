{-# LANGUAGE OverloadedStrings #-}

module Main where

import API (runServer)

main :: IO ()
main = do
    putStrLn "Bienvenido!"
    putStrLn "=============================================="
    putStrLn "El servidor se ha iniciado en http://localhost:8081\n"
    putStrLn "Endpoints disponibles:"
    putStrLn "  POST    /workflows                  -> Crea un nuevo workflow"
    putStrLn "  GET     /workflows                  -> Lista todos los workflows"
    putStrLn "  GET     /workflows/:id             -> Obtiene un workflow por ID"
    putStrLn "  DELETE  /workflows/:id             -> Elimina un workflow por ID"
    putStrLn "  POST    /workflows/:id/execution   -> Ejecuta un workflow"
    putStrLn "  GET     /workflows/:id/execution   -> Listado de ejecuciones para un workflow"
    putStrLn "  GET     /workflows/execution       -> Lista todas las ejecuciones del sistema"
    putStrLn "  POST    /tasks                     -> Sube un nuevo script de tarea (archivo)"
    putStrLn "  PATCH   /tasks                     -> Reemplaza el contenido de un script existente"
    putStrLn "  GET     /tasks                     -> Lista todos los scripts cargados"
    putStrLn "  GET     /tasks/:id                 -> Obtiene un script por su ID"
    putStrLn "  POST    /inputs                    -> Sube un archivo de input para tareas"
    putStrLn "==============================================\n"
    runServer
