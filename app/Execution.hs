{-# LANGUAGE OverloadedStrings #-}

module Execution (runExecutionPlans) where

import System.Process (readProcess)
import Control.Exception (catch, SomeException)
import Types (TaskInput(..), TaskOutput(..), ExecutionPlan(..), FailStrategy(..))
import Monad (ExecutionMonad, getState, logMsg, updateState, ExecutionState, runDB)
import Database (getTaskContentByName)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)
import Filesystem (convertToDockerPath, makeAbsolutePath, joinPath, takeExtension, takeFileName, resolveInputPath, createDirectoryIfMissingSafe, writeLazyFile)

runExecutionPlans :: [ExecutionPlan] -> ExecutionMonad Bool
runExecutionPlans plans = do
  result <- foldM runAndAccumulate True plans
  logFinalState
  return result

runAndAccumulate :: Bool -> ExecutionPlan -> ExecutionMonad Bool
runAndAccumulate False _ = return False
runAndAccumulate True plan = do
  logMsg $ "\n==> Ejecutando tarea: " ++ planTaskName plan
  state <- getState
  resolvedArgsResult <- resolveInputsFromState (planArgs plan) state
  case resolvedArgsResult of
    Left err -> do
      logMsg $ "[Input] Error resolviendo inputs: " ++ err
      return False
    Right resolvedArgs -> do
      let updatedPlan = plan { planArgs = resolvedArgs }
      executePlanWithLogging updatedPlan

executePlanWithLogging :: ExecutionPlan -> ExecutionMonad Bool
executePlanWithLogging plan = do
  result <- executeWithRetries plan (planRetries plan)
  case result of
    Right outVal -> do
      updateState (planTaskName plan) outVal
      logMsg $ "[OK] Tarea finalizada con salida: " ++ show outVal
      return True
    Left err -> do
      logMsg $ "[FAIL] Error en tarea: " ++ err
      handleFailStrategy (planFailStrategy plan)

handleFailStrategy :: FailStrategy -> ExecutionMonad Bool
handleFailStrategy FailWorkflow = logMsg "[FAIL-STRATEGY] Terminar workflow." >> return False
handleFailStrategy ContinueWorkflow = logMsg "[FAIL-STRATEGY] Continuar workflow." >> return True

logFinalState :: ExecutionMonad ()
logFinalState = do
  finalState <- getState
  logMsg "\n== Estado final del workflow =="
  mapM_ (\(t, o) -> logMsg $ " - Tarea: " ++ t ++ ", Resultado: " ++ show o) (reverse finalState)


executeWithRetries :: ExecutionPlan -> Int -> ExecutionMonad (Either String TaskOutput)
executeWithRetries plan retriesLeft = do
  logMsg $ "[RETRY] Intentos restantes para " ++ planTaskName plan ++ ": " ++ show retriesLeft
  let cmd = planCommand plan
      inputs = planArgs plan
      expected = planOutput plan
  result <- executeInDocker plan cmd inputs expected
  case result of
    Right outVal -> return $ Right outVal
    Left err ->
      if retriesLeft > 0
        then do
          logMsg $ "[RETRY] Reintentando tarea..."
          executeWithRetries plan (retriesLeft - 1)
        else return $ Left err

executeInDocker :: ExecutionPlan -> String -> [TaskInput] -> TaskOutput -> ExecutionMonad (Either String TaskOutput)
executeInDocker plan cmd inputs expectedOutput = do
  logMsg $ "[DOCKER] Comando recibido: " ++ show cmd
  let (dockerImage, interpreter) = selectDockerImage cmd
  logMsg $ "[DOCKER] Imagen: " ++ dockerImage ++ ", Intérprete: " ++ interpreter
  (_, scriptPath) <- prepareScript plan cmd
  resolvedInputs <- prepareInputs inputs
  logMsg $ "[DOCKER] Inputs resueltos: " ++ show resolvedInputs
  runInContainer plan interpreter scriptPath resolvedInputs expectedOutput

prepareScript :: ExecutionPlan -> String -> ExecutionMonad (String, FilePath)
prepareScript plan cmd = do
  let (_, interpreter) = selectDockerImage cmd
  logMsg $ "[SCRIPT] Preparando script para: " ++ cmd
  absoluteScriptPath <- liftIO $ do
    let path = joinPath "tasks" cmd
    mbs <- runDB (getTaskContentByName cmd)
    case mbs of
      Just bs -> do
        createDirectoryIfMissingSafe "tasks"
        writeLazyFile path bs
        makeAbsolutePath path
      Nothing -> do
        let tempName = "temp_" ++ sanitizeFileName (planTaskName plan) ++ ".sh"
            tempPath = joinPath "tasks" tempName
        writeFile tempPath cmd
        makeAbsolutePath tempPath
  dockerScriptPath <- liftIO $ convertToDockerPath absoluteScriptPath
  logMsg $ "[SCRIPT] Script preparado en: " ++ dockerScriptPath
  return (interpreter, dockerScriptPath)

prepareInputs :: [TaskInput] -> ExecutionMonad [FilePath]
prepareInputs inputs = do
  let files = [fp | FileInput fp <- inputs]
  results <- liftIO $ mapM resolveInputPath files
  case sequence results of
    Left err -> logAndFail "[INPUT] No se pudo resolver path de input" err >> return []
    Right paths -> return paths


runInContainer :: ExecutionPlan -> String -> FilePath -> [FilePath] -> TaskOutput -> ExecutionMonad (Either String TaskOutput)
runInContainer plan interpreter dockerScriptPath resolvedFiles expectedOutput = do
  let containerName = planTaskName plan
      scriptFile = takeFileName dockerScriptPath
  logMsg $ "[CONTAINER] Script: " ++ scriptFile ++ ", Tarea: " ++ containerName
  containerIdResult <- liftIO $ runCommand
    ["docker", "create", "--rm=false", "--name", containerName,
     "-v", dockerScriptPath ++ ":/app/" ++ scriptFile,
     fst (selectDockerImage scriptFile), "sh", "-c", "sleep infinity"]
  case containerIdResult of
    Left err -> logAndFail "[CONTAINER] Error creando contenedor" err
    Right containerId -> do
      logMsg $ "[CONTAINER] ID: " ++ containerId
      _ <- liftIO $ mapM (\file -> runCommand ["docker", "cp", file, containerId ++ ":/app/" ++ takeFileName file]) resolvedFiles
      _ <- liftIO $ runCommand ["docker", "start", containerId]
      let quotedArgs = map (\arg -> "\"" ++ getInputValue arg ++ "\"") (planArgs plan)
      let execCmd = "cd /app && " ++ interpreter ++ " " ++ scriptFile ++ " " ++ unwords quotedArgs
      logMsg $ "[DOCKER-EXEC] Ejecutando: " ++ execCmd
      execResult <- liftIO $ runCommand ["docker", "exec", "-i", containerId, "sh", "-c", execCmd]
      case execResult of
        Left err -> logAndFail "[DOCKER-EXEC] Fallo en ejecución" err <* cleanupContainer containerId
        Right outStr -> do
          logMsg $ "[DOCKER-OUTPUT] " ++ outStr
          result <- processExecutionResult expectedOutput containerId outStr
          _ <- cleanupContainer containerId
          return result

logAndFail :: String -> String -> ExecutionMonad (Either String a)
logAndFail label msg = logMsg (label ++ ": " ++ msg) >> return (Left msg)

cleanupContainer :: String -> ExecutionMonad ()
cleanupContainer cid = liftIO (runCommand ["docker", "rm", "-f", cid]) >> return ()

sanitizeFileName :: String -> String
sanitizeFileName = map (\c -> if c `elem` ("/\\:*?\"<>|" :: String) then '_' else c)

resolveInputsFromState :: [TaskInput] -> ExecutionState -> ExecutionMonad (Either String [TaskInput])
resolveInputsFromState inputs state = return $ traverse resolve inputs
  where
    resolve (VarInput ('@':ref)) = case lookup ref state of
      Just (OutputValue v) -> Right (VarInput v)
      Just (OutputFile _)  -> Left $ "Se esperaba un valor, pero " ++ ref ++ " es un archivo."
      Nothing              -> Left $ "Tarea desconocida: " ++ ref
    resolve (FileInput ('@':ref)) = case lookup ref state of
      Just (OutputFile f)  -> Right (FileInput f)
      Just (OutputValue _) -> Left $ "Se esperaba un archivo, pero " ++ ref ++ " es un valor."
      Nothing              -> Left $ "Tarea desconocida: " ++ ref
    resolve (FileInput path) = Right (FileInput ("input/" ++ path))
    resolve other = Right other

processExecutionResult :: TaskOutput -> String -> String -> ExecutionMonad (Either String TaskOutput)
processExecutionResult (OutputFile outPath) containerId _ = do
  let hostPath = "./output/" ++ takeFileName outPath
  _ <- liftIO $ runCommand ["docker", "cp", containerId ++ ":/app/" ++ takeFileName outPath, hostPath]
  return $ Right (OutputFile hostPath)
processExecutionResult (OutputValue _) _ outStr = return $ Right (OutputValue outStr)

-- Helpers
getInputValue :: TaskInput -> String
getInputValue (FileInput file) = "/app/" ++ takeFileName file
getInputValue (VarInput val)   = val

selectDockerImage :: String -> (String, String)
selectDockerImage cmd
  | takeExtension cmd == ".py"  = ("python:3.9", "python")
  | takeExtension cmd == ".js"  = ("node:18", "node")
  | takeExtension cmd == ".sh"  = ("ubuntu:latest", "bash")
  | takeExtension cmd == ".c"   = ("gcc:latest", "gcc")
  | otherwise                   = ("ubuntu:latest", "sh")

runCommand :: [String] -> IO (Either String String)
runCommand [] = return $ Left "Error: comando vacío"
runCommand (first:rest) = catch
  (do
    output <- readProcess first rest ""
    return $ Right (takeWhile (/= '\n') output))
  (\e -> return $ Left $ "Error ejecutando comando en Docker: " ++ show (e :: SomeException))
