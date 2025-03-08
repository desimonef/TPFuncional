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
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Database (DB, saveWorkflow, getWorkflows, getWorkflowById, getWorkflowByName, saveTask, getTaskById, getTasks, taskExists)
import System.Directory (createDirectoryIfMissing)
import GHC.Generics (Generic)
import Types (Workflow(..), Task(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))

-- Definición de la API
type WorkflowAPI =
       "workflows" :> ReqBody '[JSON] Workflow :> Post '[JSON] (Either String Int)
  :<|> "workflows" :> Get '[JSON] [(Int, Workflow)]
  :<|> "workflows" :> Capture "id" Int :> Get '[JSON] (Maybe (Int, Workflow))
  :<|> "workflows" :> QueryParam "name" String :> Get '[JSON] (Maybe (Int, Workflow))
  :<|> "tasks" :> MultipartForm Mem TaskUpload :> Post '[JSON] Int
  :<|> "tasks" :> Capture "id" Int :> Get '[JSON] (Maybe (Int, String))
  :<|> "tasks" :> Get '[JSON] [(Int, String)]

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
       liftIO . addWorkflow
  :<|> liftIO listWorkflows
  :<|> liftIO . getWorkflowByIdAPI
  :<|> liftIO . getWorkflowByNameAPI
  :<|> liftIO . addTask
  :<|> liftIO . getTaskByIdAPI
  :<|> liftIO listTasks
  where
      addWorkflow :: Workflow -> IO (Either String Int)
      addWorkflow wf = do
          let scriptTasks = [script | Task { script = Just script } <- tasks wf]
          allExists <- allM (taskExists db) scriptTasks
          if allExists
              then Right <$> saveWorkflow db wf
              else return $ Left "Some script tasks are not registered in the database."

      listWorkflows :: IO [(Int, Workflow)]
      listWorkflows = getWorkflows db

      getWorkflowByIdAPI :: Int -> IO (Maybe (Int, Workflow))
      getWorkflowByIdAPI = getWorkflowById db

      getWorkflowByNameAPI :: Maybe String -> IO (Maybe (Int, Workflow))
      getWorkflowByNameAPI (Just name) = getWorkflowByName db name
      getWorkflowByNameAPI Nothing = return Nothing

      addTask :: TaskUpload -> IO Int
      addTask (TaskUpload file) = do
          let fileName = T.unpack $ fdFileName file
              dirPath = "tasks"
              filePath = dirPath </> fileName
          createDirectoryIfMissing True dirPath  -- Asegura que el directorio existe
          BL.writeFile filePath (fdPayload file)
          saveTask db fileName filePath

      getTaskByIdAPI :: Int -> IO (Maybe (Int, String))
      getTaskByIdAPI = getTaskById db

      listTasks :: IO [(Int, String)]
      listTasks = getTasks db

-- Función para evaluar si todos los elementos cumplen con una condición
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = fmap and . mapM p

-- Función para levantar el servidor
runServer :: DB -> IO ()
runServer db = do
    run 8081 (serve (Proxy :: Proxy WorkflowAPI) (server db))
