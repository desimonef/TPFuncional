{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module API (runServer) where

import Servant
import Servant.Multipart
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Control.Monad (filterM)
import Data.List ((\\))
import Data.Time.Clock (getCurrentTime)
import Types (Workflow(..), Task(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..))
import Workflows (executeWorkflow)
import Monad (runDB, runExecutionMonad)
import Database (saveWorkflow, getWorkflows, getWorkflowById, saveTask, getTaskById, getTasks, taskExists, saveExecution, getAllExecutions, getExecutionsByWorkflow, workflowNameExists, deleteWorkflow, replaceTaskContent, initDB)
import Serialization (decodeJSONText, jsonErrorBody)
import Filesystem (createDirectoryIfMissingSafe, writeLazyFile, joinPath, textToFilePath)
import Execution(runExecutionPlans)






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
  :<|> "workflows" :> Capture "id" Int :> Delete '[JSON] NoContent
  :<|> "inputs" :> MultipartForm Mem InputUpload :> Post '[JSON] IdResponse
  :<|> "tasks" :> MultipartForm Mem TaskUpload :> Patch '[JSON] NoContent





data TaskUpload = TaskUpload { taskFile :: FileData Mem }
  deriving (Generic)

instance FromMultipart Mem TaskUpload where
    fromMultipart form = TaskUpload <$> lookupFile "taskFile" form


data InputUpload = InputUpload { inputFile :: FileData Mem } deriving (Generic)

instance FromMultipart Mem InputUpload where
    fromMultipart form = InputUpload <$> lookupFile "inputFile" form

server :: Server WorkflowAPI
server =
       addWorkflow
  :<|> listWorkflows
  :<|> getWorkflowByIdAPI
  :<|> addTask
  :<|> getTaskByIdAPI
  :<|> listTasks
  :<|> executeWorkflowAPI
  :<|> getExecutionsByWorkflowAPI
  :<|> getAllExecutionsAPI
  :<|> deleteWorkflowAPI
  :<|> uploadInputFile
  :<|> patchTask


  where
      addWorkflow :: Workflow -> Handler IdResponse
      addWorkflow wf = do
        exists <- liftIO $ runDB (workflowNameExists (workflow_name wf))
        if exists
            then throwError err409 { errBody = jsonErrorBody "Ya existe un workflow con ese nombre." }
            else do
            let scripts = [s | Task { script = Just s } <- tasks wf]
            found <- liftIO $ filterM (runDB . taskExists) scripts
            let missing = scripts \\ found
            if null missing
                then IdResponse <$> liftIO (runDB (saveWorkflow wf))
                else throwError err400 { errBody = jsonErrorBody ("These script tasks are missing: " ++ show missing) }



      listWorkflows :: Handler [WorkflowResponse]
      listWorkflows = do
        result <- liftIO $ runDB getWorkflows
        return $ map (\(wid, wfName, def) -> WorkflowResponse wid wfName def) result

      getWorkflowByIdAPI :: Int -> Handler WorkflowResponse
      getWorkflowByIdAPI wfId = do
        result <- liftIO $ runDB (getWorkflowById wfId)
        case result of
            Just (wid, wfName, def) -> return $ WorkflowResponse wid wfName def
            Nothing -> throwError err404 { errBody = "Workflow not found" }

      addTask :: TaskUpload -> Handler IdResponse
      addTask (TaskUpload file) = do
          let fileName = textToFilePath (fdFileName file)
              content = fdPayload file
          IdResponse <$> liftIO (runDB (saveTask fileName content))

      getTaskByIdAPI :: Int -> Handler (Int, String)
      getTaskByIdAPI tid = do
          result <- liftIO $ runDB (getTaskById tid)
          case result of
              Just task -> return task
              Nothing -> throwError err404 { errBody = "Task not found" }

      listTasks :: Handler [(Int, String)]
      listTasks = liftIO $ runDB getTasks

      executeWorkflowAPI :: Int -> Handler ExecutionResponse
      executeWorkflowAPI wfId = do
        maybeWf <- liftIO $ runDB (getWorkflowById wfId)
        case maybeWf of
            Nothing -> throwError err404
            Just (_, _, rawDef) -> do
                liftIO $ putStrLn $ "Definición JSON recibida (raw): " ++ show rawDef
                case decodeJSONText rawDef :: Maybe Workflow of
                    Nothing -> throwError err500 { errBody = "Workflow malformado: JSON inválido" }
                    Just wf -> do
                        currentTime <- liftIO getCurrentTime
                        _ <- liftIO $ runDB (saveExecution wfId currentTime)
                        case executeWorkflow wf of
                            Left err -> return $ ExecutionResponse False [err]
                            Right plans -> do
                                (ok, logLines) <- liftIO $ runExecutionMonad (runExecutionPlans plans)
                                return $ ExecutionResponse ok logLines

      getExecutionsByWorkflowAPI :: Int -> Handler [ExecutionRecord]
      getExecutionsByWorkflowAPI wfId = do
        rows <- liftIO $ runDB (getExecutionsByWorkflow wfId)
        return $ map (\(eid, wid, ts, st) -> ExecutionRecord eid wid ts st) rows

      getAllExecutionsAPI :: Handler [ExecutionRecord]
      getAllExecutionsAPI = do
        rows <- liftIO $ runDB getAllExecutions
        return $ map (\(eid, wid, ts, st) -> ExecutionRecord eid wid ts st) rows

      deleteWorkflowAPI :: Int -> Handler NoContent
      deleteWorkflowAPI wid = do
          deleted <- liftIO $ runDB (deleteWorkflow wid)
          if deleted then return NoContent
          else throwError err404 { errBody = jsonErrorBody "Workflow no encontrado para eliminar" }

      uploadInputFile :: InputUpload -> Handler IdResponse
      uploadInputFile (InputUpload file) = do
          let fileName = fdFileName file
          liftIO $ do
              createDirectoryIfMissingSafe "./input"
              writeLazyFile (joinPath "./input" (textToFilePath fileName)) (fdPayload file)
          return $ IdResponse 0

      patchTask :: TaskUpload -> Handler NoContent
      patchTask (TaskUpload file) = do
        let fileName = textToFilePath (fdFileName file)
            content = fdPayload file
        exists <- liftIO $ runDB (taskExists fileName)
        if exists
            then do
            liftIO $ runDB (replaceTaskContent fileName content)
            return NoContent
            else throwError err404 { errBody = jsonErrorBody "Task no encontrada para actualizar" }
    
  


runServer :: IO ()
runServer = do
    initDB
    run 8081 (serve (Proxy :: Proxy WorkflowAPI) server)
