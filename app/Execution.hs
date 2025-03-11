{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Execution (executeScript, executeWithRetries, checkCondition) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, withObject, Value(..), (.:?), parseJSON)
import System.Process (readProcess)
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)
import Data.Text (unpack)
import Types (Task(..), ExecutionState(..), TaskOutput(..), Condition(..))
import Language.Haskell.Interpreter hiding (name)

executeScript :: Task -> [String] -> IO (Either String TaskOutput)
executeScript task args = catch
    (do
        putStrLn $ "Ejecutando tarea: " ++ name task
        case (command task, script task, output task) of
            (Just cmd, Nothing, Just (OutputFile file)) -> do
                output <- readProcess cmd args ""
                writeFile file output
                putStrLn $ "Archivo generado: " ++ file
                return $ Right (OutputFile file)

            (Just cmd, Nothing, _) -> do
                output <- readProcess cmd args ""
                putStrLn $ "Output capturado de " ++ name task ++ ": " ++ output
                return $ Right (OutputValue output) -- 🔹 Guardar el output real

            (Nothing, Just file, Just (OutputFile outFile)) -> do
                let fullCommand = if takeExtension file == ".bat"
                                  then ["cmd.exe", "/c", file] ++ args
                                  else [selectInterpreter file, file] ++ args
                output <- readProcess (head fullCommand) (tail fullCommand) ""  
                writeFile outFile output
                putStrLn $ "Archivo generado: " ++ outFile
                return $ Right (OutputFile outFile)

            (Nothing, Just file, _) -> do
                let fullCommand = if takeExtension file == ".bat"
                                  then ["cmd.exe", "/c", file] ++ args
                                  else [selectInterpreter file, file] ++ args
                output <- readProcess (head fullCommand) (tail fullCommand) ""  
                putStrLn $ "Output capturado de " ++ name task ++ ": " ++ output
                return $ Right (OutputValue output)) -- 🔹 Guardar el output real
    
    (\e -> return $ Left $ "Error ejecutando tarea " ++ name task ++ ": " ++ show (e :: SomeException))


executeWithRetries :: Task -> [String] -> Int -> IO (Either String TaskOutput)
executeWithRetries task args remainingRetries = do
    result <- executeScript task args
    case result of
        Right output -> return $ Right output
        Left err -> 
            if remainingRetries > 0 then do
                putStrLn $ "Reintentando tarea " ++ name task ++ "... (" ++ show remainingRetries ++ " intentos restantes)"
                executeWithRetries task args (remainingRetries - 1)
            else return $ Left err


selectInterpreter :: FilePath -> String
selectInterpreter path =
    case takeExtension path of
        ".py"  -> "python3"
        ".js"  -> "node"
        ".sh"  -> "bash"
        ".bat" -> "cmd.exe"  -- 🔹 Para Windows
        _      -> error $ "No se reconoce el tipo de archivo: " ++ path


-- Elimina el salto de línea final del output
stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse


checkCondition :: Task -> [(String, String)] -> IO Bool
checkCondition task state =
    case condition task of
        AlwaysRun -> return True
        SuccessCondition prevTask ->
            return $ case lookup prevTask state of
                Just _ -> True
                Nothing -> False
        OutputValueCondition prevTask condStr ->
            case lookup prevTask state of
                Just value -> evaluateCondition condStr value
                Nothing -> return False
        OutputFileCondition prevTask condStr ->
            case lookup prevTask state of
                Just filePath -> evaluateFileCondition condStr filePath
                Nothing -> return False


evaluateCondition :: String -> String -> IO Bool
evaluateCondition conditionCode inputValue = do
    result <- runInterpreter $ do
        setImports ["Prelude"]
        interpret ("(" ++ conditionCode ++ ") " ++ show inputValue) (as :: Bool)
    case result of
        Left err -> do
            putStrLn $ "Error evaluando condición: " ++ show err
            return False
        Right val -> return val

evaluateFileCondition :: String -> String -> IO Bool
evaluateFileCondition conditionCode filePath = do
    result <- runInterpreter $ do
        setImports ["Prelude", "System.Directory"]
        interpret ("(" ++ conditionCode ++ ") " ++ show filePath) (as :: IO Bool)
    case result of
        Left err -> do
            putStrLn $ "Error evaluando condición de archivo: " ++ show err
            return False
        Right action -> action  -- Ejecuta la acción IO Bool
