{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API (runServer) where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)
import Database (DB, saveWorkflow, getWorkflows, getWorkflow, updateWorkflowStatus, getWorkflowStatus)
import Workflows (executeWorkflow)
import Types (Workflow(..))

-- Definición de la API
type WorkflowAPI =
       "workflows" :> ReqBody '[JSON] Workflow :> Post '[JSON] String
  :<|> "workflows" :> Get '[JSON] [String]
  :<|> "workflows" :> Capture "name" String :> Get '[JSON] (Maybe Workflow)
  :<|> "workflows" :> Capture "name" String :> "run" :> Post '[JSON] String
  :<|> "workflows" :> Capture "name" String :> "status" :> Get '[JSON] String

-- Implementación de los endpoints
server :: DB -> Server WorkflowAPI
server db =
       liftIO . addWorkflow
  :<|> liftIO listWorkflows
  :<|> liftIO . getWorkflowByName
  :<|> liftIO . runWorkflow
  :<|> liftIO . getWorkflowStatusAPI
  where
      addWorkflow :: Workflow -> IO String
      addWorkflow wf = do
          saveWorkflow db wf
          return $ "Workflow " ++ workflow_name wf ++ " agregado"

      listWorkflows :: IO [String]
      listWorkflows = getWorkflows db

      getWorkflowByName :: String -> IO (Maybe Workflow)
      getWorkflowByName name = getWorkflow db name

      runWorkflow :: String -> IO String
      runWorkflow name = do
          updateWorkflowStatus db name "running"
          executeWorkflow db (Workflow name [])  -- 🔹 Aquí corregimos el error del Workflow vacío
          updateWorkflowStatus db name "completed"
          return $ "Workflow " ++ name ++ " finalizado"

      getWorkflowStatusAPI :: String -> IO String
      getWorkflowStatusAPI name = getWorkflowStatus db name

-- Función para levantar el servidor
runServer :: DB -> IO ()
runServer db = do
    run 8081 (serve (Proxy :: Proxy WorkflowAPI) (server db))

