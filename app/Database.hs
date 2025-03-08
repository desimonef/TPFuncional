{-# LANGUAGE OverloadedStrings #-}

module Database (DB, initDB, saveWorkflow, getWorkflows, getWorkflow, updateWorkflowStatus, getWorkflowStatus) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Types (Workflow(..))

type DB = Connection

-- Conexión y creación de tablas
initDB :: IO DB
initDB = do
    conn <- open "workflows.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS workflows (name TEXT PRIMARY KEY, definition TEXT, status TEXT)"
    return conn

-- Guardar un workflow en la base de datos
saveWorkflow :: DB -> Workflow -> IO ()
saveWorkflow conn wf = do
    let jsonDef = TE.decodeUtf8 . BL.toStrict $ encode wf  -- 🔹 Convertir a Text
    execute conn "INSERT INTO workflows (name, definition, status) VALUES (?, ?, ?) ON CONFLICT(name) DO UPDATE SET definition = ?" 
        (workflow_name wf, jsonDef, T.pack "pending", jsonDef)  -- 🔹 Convertir "pending" a Text

-- Obtener todos los workflows
getWorkflows :: DB -> IO [String]
getWorkflows conn = do
    rows <- query_ conn "SELECT name FROM workflows" :: IO [Only String]
    return $ map fromOnly rows

-- Obtener un workflow por nombre
getWorkflow :: DB -> String -> IO (Maybe Workflow)
getWorkflow conn name = do
    rows <- query conn "SELECT definition FROM workflows WHERE name = ?" (Only name) :: IO [Only T.Text]
    return $ case rows of
        [Only jsonDef] -> decode (BL.fromStrict (TE.encodeUtf8 jsonDef))
        _ -> Nothing

updateWorkflowStatus :: DB -> String -> String -> IO String
updateWorkflowStatus db name status = return $ "Update Status " ++ name ++ " - " ++ status

getWorkflowStatus :: DB -> String -> IO String
getWorkflowStatus db name = return $ "Get Status " ++ name ++ " status"