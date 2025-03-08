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
import Database (DB, saveWorkflow, getWorkflows, getWorkflowById, getWorkflowByName, saveTask, getTaskById)
import System.Directory (copyFile)
import GHC.Generics (Generic)
import Types (Workflow(..), Task(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))

-- Definición de la API
type WorkflowAPI =
       "workflows" :> ReqBody '[JSON] Workflow :> Post '[JSON] Int
  :<|> "workflows" :> Get '[JSON] [Workflow]
  :<|> "workflows" :> Capture "id" Int :> Get '[JSON] (Maybe Workflow)
  :<|> "workflows" :> QueryParam "name" String :> Get '[JSON] (Maybe Workflow)
  :<|> "tasks" :> MultipartForm Mem TaskUpload :> Post '[JSON] Int
  :<|> "tasks" :> Capture "id" Int :> Get '[JSON] (Maybe Task)

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
  where
      addWorkflow :: Workflow -> IO Int
      addWorkflow wf = saveWorkflow db wf

      listWorkflows :: IO [Workflow]
      listWorkflows = getWorkflows db

      getWorkflowByIdAPI :: Int -> IO (Maybe Workflow)
      getWorkflowByIdAPI = getWorkflowById db

      getWorkflowByNameAPI :: Maybe String -> IO (Maybe Workflow)
      getWorkflowByNameAPI (Just name) = getWorkflowByName db name
      getWorkflowByNameAPI Nothing = return Nothing

      addTask :: TaskUpload -> IO Int
      addTask (TaskUpload file) = do
          let fileName = T.unpack $ fdFileName file
              filePath = "tasks" </> fileName
          BL.writeFile filePath (fdPayload file)
          saveTask db fileName filePath

      getTaskByIdAPI :: Int -> IO (Maybe Task)
      getTaskByIdAPI = getTaskById db

-- Función para levantar el servidor
runServer :: DB -> IO ()
runServer db = do
    run 8081 (serve (Proxy :: Proxy WorkflowAPI) (server db))