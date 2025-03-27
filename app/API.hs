{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module API (runServer) where

import Servant
import Servant.Multipart
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Database (DB, saveWorkflow, getWorkflows, getWorkflowById, saveTask, getTaskById, getTasks, taskExists, saveExecution, getAllExecutions, getExecutionsByWorkflow, updateExecutionStatus)
import System.Directory (createDirectoryIfMissing)
import GHC.Generics (Generic)
import Types (Workflow(..), Task(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import Control.Monad (filterM)
import Data.List ((\\))
import Workflows (executeWorkflow)
import Serialization (encodeJSON, decodeJSON)
import Data.Time.Clock (getCurrentTime)

-- Definición de la API con respuestas HTTP adecuadas
type WorkflowAPI =
       "workflows" :> ReqBody '[JSON] Workflow :> Post '[JSON] IdResponse
  :<|> "workflows" :> Get '[JSON] [WorkflowResponse]
  :<|> "workflows" :> Capture "id" Int :> Get '[JSON] WorkflowResponse
  :<|> "tasks" :> MultipartForm Mem TaskUpload :> Post '[JSON] IdResponse
  :<|> "tasks" :> Capture "id" Int :> Get '[JSON] (Int, String)
  :<|> "tasks" :> Get '[JSON] [(Int, String)]
  :<|> "workflows" :> Capture "id" Int :> "execution" :> Post '[JSON] ExecutionResponse
  :<|> "workflows" :> Capture "id" Int :> "execution" :> Get '[JSON] [ExecutionRecord]
  :<|> "workflows" :> "execution" :> Get '[JSON] [ExecutionRecord]

-- Tipo para recibir archivos
data TaskUpload = TaskUpload { taskFile :: FileData Mem }
  deriving (Generic)

instance FromMultipart Mem TaskUpload where
    fromMultipart form = case lookupFile "taskFile" form of
        Right file -> Right (TaskUpload file)
        Left err   -> Left err

-- Implementación de los endpoints
server :: DB -> Server WorkflowAPI
server db =
       addWorkflow
  :<|> listWorkflows
  :<|> getWorkflowByIdAPI
  :<|> addTask
  :<|> getTaskByIdAPI
  :<|> listTasks
  :<|> executeWorkflowAPI
  :<|> getExecutionsByWorkflowAPI
  :<|> getAllExecutionsAPI
  where
      addWorkflow :: Workflow -> Handler IdResponse
      addWorkflow wf = do
          let scriptTasks = [script | Task { script = Just script } <- tasks wf]
          existingTasks <- liftIO $ filterM (taskExists db) scriptTasks
          let missingTasks = scriptTasks \\ existingTasks
          if null missingTasks
              then IdResponse <$> liftIO (saveWorkflow db wf)
              else throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack ("These script tasks are missing: " ++ show missingTasks))) }

      listWorkflows :: Handler [WorkflowResponse]
      listWorkflows = do
          workflows <- liftIO $ getWorkflows db
          return $ map (\(wid, name, def) -> WorkflowResponse wid name def) workflows

      getWorkflowByIdAPI :: Int -> Handler WorkflowResponse
      getWorkflowByIdAPI wid = do
          result <- liftIO $ getWorkflowById db wid
          case result of
              Just (wid, name, def) -> return $ WorkflowResponse wid name def
              Nothing -> throwError err404 { errBody = "Workflow not found" }

      addTask :: TaskUpload -> Handler IdResponse
      addTask (TaskUpload file) = do
          let dir = "./tasks"  
              fileName = T.unpack $ fdFileName file
              filePath = dir </> fileName  

          liftIO $ do
              createDirectoryIfMissing True dir  
              BL.writeFile filePath (fdPayload file)  
    
          IdResponse <$> liftIO (saveTask db fileName) 

      getTaskByIdAPI :: Int -> Handler (Int, String)
      getTaskByIdAPI tid = do
          result <- liftIO $ getTaskById db tid
          case result of
              Just task -> return task
              Nothing -> throwError err404 { errBody = "Task not found" }

      listTasks :: Handler [(Int, String)]
      listTasks = liftIO $ getTasks db

      executeWorkflowAPI :: Int -> Handler ExecutionResponse
      executeWorkflowAPI wid = do
            result <- liftIO $ getWorkflowById db wid
            case result of
                Just (_, _, def) -> case decodeJSON (BL.fromStrict (TE.encodeUtf8 def)) :: Maybe Workflow of
                    Just workflow -> do
                        -- Guardar ejecución con estado "started"
                        timestamp <- liftIO getCurrentTime
                        execId <- liftIO $ saveExecution db wid timestamp

                        -- Ejecutar el workflow y obtener si fue exitoso o falló
                        success <- liftIO $ executeWorkflow db workflow

                        -- Determinar el estado final
                        let finalStatus = if success then "completed" else "failed"

                        -- Actualizar el estado en la BD
                        liftIO $ updateExecutionStatus db execId finalStatus

                        -- Responder a la API
                        return $ ExecutionResponse (if success then "Execution completed" else "Execution failed")
                    Nothing -> throwError err400 { errBody = "Invalid workflow format" }
                Nothing -> throwError err404 { errBody = "Workflow not found" }



      getExecutionsByWorkflowAPI :: Int -> Handler [ExecutionRecord]
      getExecutionsByWorkflowAPI wid = do
          executions <- liftIO $ getExecutionsByWorkflow db wid
          return $ map (\(eid, wid, ts, status) -> ExecutionRecord eid wid ts status) executions

      getAllExecutionsAPI :: Handler [ExecutionRecord]
      getAllExecutionsAPI = do
          executions <- liftIO $ getAllExecutions db
          return $ map (\(eid, wid, ts, status) -> ExecutionRecord eid wid ts status) executions



-- Función para levantar el servidor
runServer :: DB -> IO ()
runServer db = do
    run 8081 (serve (Proxy :: Proxy WorkflowAPI) (server db))
