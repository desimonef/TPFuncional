{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript, executeWithRetries) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, Value(..), Object, (.:?), parseJSON, (.:), decode)
import qualified Data.Aeson.KeyMap as KM
import System.Process (readProcess)
import System.FilePath (takeExtension, takeFileName, takeDrive, dropDrive, (</>))
import System.Directory (doesFileExist, makeAbsolute)
import System.Info (os)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), TaskInput(..), TaskOutput(..), ExecutionPlan(..))
import Monad(ExecutionMonad, getState, logMsg, updateState)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as B

-- 🔹 Construye el ExecutionPlan a partir de una Task
buildExecutionPlan :: Task -> [TaskInput] -> Maybe ExecutionPlan
buildExecutionPlan task args =
    case (name task, command task, script task, taskToTaskOutput task) of
        (taskName, Just cmd, Nothing, outFile) -> Just (ExecutionPlan taskName cmd args outFile)
        (taskName, Nothing, Just file, outFile) -> Just (ExecutionPlan taskName file args outFile)
        _ -> Nothing

executeScript :: Task -> [TaskInput] -> ExecutionMonad (Either String TaskOutput)
executeScript task args = do
    logMsg $ "Preparando ejecución de tarea: " ++ name task
    case buildExecutionPlan task args of
        Just plan -> do
            manageContainer plan
        Nothing -> do
            logMsg "Error: Tarea mal definida, debe tener `command` o `script`, pero no ambos."
            return $ Left "Tarea mal definida"


executeWithRetries :: Task -> [TaskInput] -> Int -> ExecutionMonad (Either String TaskOutput)
executeWithRetries task args remainingRetries = do
    logMsg $ "Ejecutando tarea: " ++ name task
    result <- executeScript task args  

    case result of
        Right output -> do
            logMsg $ "Tarea " ++ name task ++ " ejecutada exitosamente."
            return $ Right output
        Left err -> 
            if remainingRetries > 0 then do
                logMsg $ "Error en tarea " ++ name task ++ ": " ++ err
                logMsg $ "Reintentando tarea " ++ name task ++ "... (" ++ show remainingRetries ++ " intentos restantes)"
                executeWithRetries task args (remainingRetries - 1)
            else do
                logMsg $ "Error crítico en tarea " ++ name task ++ ", no hay más intentos disponibles."
                return $ Left err

-- 🔹 Selecciona la imagen de Docker y el intérprete según el tipo de script o comando
selectDockerImage :: String -> (String, String)
selectDockerImage cmd
    | takeExtension cmd == ".py"  = ("python:3.9", "python")
    | takeExtension cmd == ".js"  = ("node:18", "node")
    | takeExtension cmd == ".sh"  = ("ubuntu:latest", "bash")
    | takeExtension cmd == ".c"   = ("gcc:latest", "gcc")
    | otherwise                   = ("ubuntu:latest", "sh")


convertToDockerPath :: FilePath -> IO FilePath
convertToDockerPath path = do
    absPath <- makeAbsolute path  -- Convierte la ruta en absoluta primero
    return $ case os of
        "mingw32" -> toDockerWindowsPath absPath
        "cygwin"  -> toDockerWindowsPath absPath
        _         -> absPath  -- En Linux/macOS no cambia

-- Función auxiliar para Windows
toDockerWindowsPath :: FilePath -> FilePath
toDockerWindowsPath path =
    let drive = map toLower (take 1 path)  -- Extrae la letra de unidad (Ej: "C")
        rest  = drop 2 path  -- Quita "C:\" dejando solo "\Users\..."
    in "/" ++ drive ++ map (\c -> if c == '\\' then '/' else c) rest  -- Convierte a "/c/Users/..."




manageContainer :: ExecutionPlan -> ExecutionMonad (Either String TaskOutput)
manageContainer plan = do
    let command = execCommand plan
    let (dockerImage, interpreter) = selectDockerImage command

    -- Convertir rutas a absolutas y en formato Docker
    absoluteScriptPath <- liftIO $ makeAbsolute ("tasks" </> command)
    dockerScriptPath <- liftIO $ convertToDockerPath absoluteScriptPath  
    let containerName = taskName plan

    let inputs = execArgs plan 
    let fileInputs = [filePath | FileInput filePath <- inputs]
    logMsg $ "fileInputs: " ++ show fileInputs

    containerIdResult <- liftIO $ runCommand 
        ["create", "--rm=false", "--name", containerName, "-v", dockerScriptPath ++ ":/app/" ++ takeFileName command, dockerImage, "sh", "-c", "sleep infinity"]

    case containerIdResult of
        Left err -> do
            logMsg $ "Error creando contenedor: " ++ err
            return $ Left err
        Right containerId -> do
            logMsg $ "Contenedor creado con ID: " ++ containerId

            _ <- liftIO $ mapM (\file -> do
                let cpCommand = ["cp", file, containerId ++ ":/app/" ++ takeFileName file]
                liftIO $ putStrLn $ "Ejecutando: " ++ unwords cpCommand
                runCommand cpCommand
                ) fileInputs

            -- 🔹 Paso 3: Iniciar el contenedor
            _ <- liftIO $ runCommand ["start", containerId]
            logMsg $ "Contenedor iniciado: " ++ containerId

            -- 🔹 Paso 4: Ejecutar el script dentro del contenedor
            let args = [getInputValue input | input <- inputs]
            let scriptExecutionCmd = interpreter ++ " /app/" ++ takeFileName command ++ " " ++ unwords args
            logMsg $ "Ejecutando script en contenedor: docker exec -i " ++ containerId ++ " sh -c " ++ show scriptExecutionCmd

            execResult <- liftIO $ runCommand  ["exec", "-i", containerId, "sh", "-c", scriptExecutionCmd]

            case execResult of
                Left err -> do
                    logMsg $ "Error ejecutando script en contenedor: " ++ err
                    return $ Left err
                Right logs -> do
                    logMsg $ "Output del contenedor: " ++ logs

                    -- 🔹 Paso 5: Capturar el resultado y procesarlo
                    result <- processExecutionResult plan containerId logs

                    -- 🔹 Paso 6: Eliminar el contenedor después de la ejecución (en caso de éxito)
                    _ <- liftIO $ runCommand ["rm", "-f", containerId]
                    logMsg $ "Contenedor eliminado: " ++ containerId

                    return result



getInputValue :: TaskInput -> String
getInputValue (FileInput file) = "/app/" ++ takeFileName file  -- Se pasa la ruta del archivo dentro del contenedor
getInputValue (VarInput var)   = var  -- Se pasa directamente el valor si es una variable

        

extractJSONResult :: B.ByteString -> Maybe String
extractJSONResult output = do 
    jsonObject <- decode output :: Maybe Object
    value <- KM.lookup "result" jsonObject  -- Uso correcto de KeyMap.lookup
    pure (valueToString value)


valueToString :: Value -> String
valueToString (String s)  = unpack s
valueToString (Number n)  = show n
valueToString (Bool b)    = show b
valueToString Null        = "null"
valueToString (Array a)   = show a
valueToString (Object o)  = show o
    
taskToTaskOutput :: Task -> TaskOutput
taskToTaskOutput task = buildTaskOutput (output task)

buildTaskOutput :: Maybe String -> TaskOutput
buildTaskOutput (Just output) = OutputFile output
buildTaskOutput Nothing       = OutputValue ""


processExecutionResult :: ExecutionPlan -> String -> String -> ExecutionMonad (Either String TaskOutput)
processExecutionResult plan containerId logs = do 
    logMsg $ "Procesando resultado: " ++ logs
    logMsg $ "ExecOutput: " ++ show (execOutput plan)

    case execOutput plan of
        -- 🔹 Si el output es un archivo, copiarlo del contenedor al host
        OutputFile filePath -> do
            logMsg $ "Copiando archivo de salida desde el contenedor: " ++ filePath
            let hostPath = "./output/" ++ takeFileName filePath
            let cpCommand = ["cp", containerId ++ ":/" ++ takeFileName filePath, hostPath]
            logMsg $ "Archivo guardado en: " ++ hostPath
            _ <- liftIO $ runCommand cpCommand

            logMsg $ "Ejecutando: " ++ unwords cpCommand

            return $ Right (OutputFile hostPath)

        -- 🔹 Si es un resultado numérico, extraerlo del JSON
        OutputValue _ -> do
            logMsg "Procesando OutputValue"
            case extractJSONResult (B.pack logs) of
                Just result -> return $ Right (OutputValue result)
                Nothing     -> return $ Left "Error: la salida de la tarea no es un JSON válido con { result: x }"



runCommand :: [String] -> IO (Either String String)
runCommand args = catch
    (do
        output <- readProcess "docker" args ""
        let trimmedOutput = takeWhile (/= '\n') output  -- 🔹 Elimina el salto de línea
        return $ Right trimmedOutput)  -- 🔹 Devuelve el container ID limpio
    (\e -> return $ Left $ "Error ejecutando comando en Docker: " ++ show (e :: SomeException))
