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
import Monad (runDB)
import Database (DB, saveWorkflow, getWorkflows, getWorkflowById, saveTask, getTaskById, getTasks, taskExists, saveExecution, getAllExecutions, getExecutionsByWorkflow, updateExecutionStatus)
import GHC.Generics (Generic)
import Types (Workflow(..), Task(..), IdResponse(..), WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..))
import Control.Monad (filterM)
import Data.List ((\\))
import Workflows (executeWorkflow)
import Serialization (decodeJSONText, jsonErrorBody)
import Data.Time.Clock (getCurrentTime)
import Filesystem (createDirectoryIfMissingSafe, writeLazyFile, joinPath, textToFilePath)


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

data TaskUpload = TaskUpload { taskFile :: FileData Mem }
  deriving (Generic)

instance FromMultipart Mem TaskUpload where
    fromMultipart form = TaskUpload <$> lookupFile "taskFile" form

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
  where
      addWorkflow :: Workflow -> Handler IdResponse
      addWorkflow wf = do
          let scriptTasks = [script | Task { script = Just script } <- tasks wf]
          existingTasks <- liftIO $ filterM (runDB . taskExists) scriptTasks
          let missingTasks = scriptTasks \\ existingTasks
          if null missingTasks
              then IdResponse <$> liftIO (runDB (saveWorkflow wf))
              else throwError err400 { errBody = jsonErrorBody ("These script tasks are missing: " ++ show missingTasks) }

      listWorkflows :: Handler [WorkflowResponse]
      listWorkflows = do
          workflows <- liftIO $ runDB getWorkflows
          return $ map (\(wid, name, def) -> WorkflowResponse wid name def) workflows

      getWorkflowByIdAPI :: Int -> Handler WorkflowResponse
      getWorkflowByIdAPI wid = do
          result <- liftIO $ runDB (getWorkflowById wid)
          case result of
              Just (wid, name, def) -> return $ WorkflowResponse wid name def
              Nothing -> throwError err404 { errBody = "Workflow not found" }

      addTask :: TaskUpload -> Handler IdResponse
      addTask (TaskUpload file) = do
          let fileName = fdFileName file
          liftIO $ do
              createDirectoryIfMissingSafe "./tasks"
              writeLazyFile (joinPath "./tasks" (textToFilePath fileName)) (fdPayload file)
          IdResponse <$> liftIO (runDB (saveTask (textToFilePath fileName)))

      getTaskByIdAPI :: Int -> Handler (Int, String)
      getTaskByIdAPI tid = do
          result <- liftIO $ runDB (getTaskById tid)
          case result of
              Just task -> return task
              Nothing -> throwError err404 { errBody = "Task not found" }

      listTasks :: Handler [(Int, String)]
      listTasks = liftIO $ runDB getTasks

      executeWorkflowAPI :: Int -> Handler ExecutionResponse
      executeWorkflowAPI wid = do
          result <- liftIO $ runDB (getWorkflowById wid)
          case result of
              Just (_, _, def) -> case decodeJSONText def :: Maybe Workflow of
                  Just workflow -> do
                      timestamp <- liftIO getCurrentTime
                      execId <- liftIO $ runDB (saveExecution wid timestamp)
                      success <- liftIO $ executeWorkflow workflow
                      let finalStatus = if success then "completed" else "failed"
                      liftIO $ runDB (updateExecutionStatus execId finalStatus)
                      return $ ExecutionResponse (if success then "Execution completed" else "Execution failed")
                  Nothing -> throwError err400 { errBody = "Invalid workflow format" }
              Nothing -> throwError err404 { errBody = "Workflow not found" }

      getExecutionsByWorkflowAPI :: Int -> Handler [ExecutionRecord]
      getExecutionsByWorkflowAPI wid = do
          executions <- liftIO $ runDB (getExecutionsByWorkflow wid)
          return $ map (\(eid, wid, ts, status) -> ExecutionRecord eid wid ts status) executions

      getAllExecutionsAPI :: Handler [ExecutionRecord]
      getAllExecutionsAPI = do
          executions <- liftIO $ runDB getAllExecutions
          return $ map (\(eid, wid, ts, status) -> ExecutionRecord eid wid ts status) executions

runServer :: IO ()
runServer = run 8081 (serve (Proxy :: Proxy WorkflowAPI) server)
