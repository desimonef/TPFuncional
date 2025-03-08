{-# LANGUAGE OverloadedStrings #-}

module Database (DB, initDB, saveWorkflow, getWorkflows, getWorkflowById, getWorkflowByName, saveTask, getTaskById) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Types (Workflow(..), Task(..))

type DB = Connection

-- Conexión y creación de tablas
initDB :: IO DB
initDB = do
    conn <- open "workflows.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS workflows (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE, definition TEXT, status TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE, file_path TEXT)"
    return conn

-- Guardar un workflow en la base de datos
saveWorkflow :: DB -> Workflow -> IO Int
saveWorkflow conn wf = do
    let jsonDef = TE.decodeUtf8 . BL.toStrict $ encode wf
    execute conn "INSERT INTO workflows (name, definition, status) VALUES (?, ?, ?)" 
        (workflow_name wf, jsonDef, T.pack "pending")
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId)

-- Obtener todos los workflows
getWorkflows :: DB -> IO [Workflow]
getWorkflows conn = do
    rows <- query_ conn "SELECT id, name, definition FROM workflows" :: IO [(Int, T.Text, T.Text)]
    return [ Workflow { workflow_name = T.unpack name, tasks = maybe [] id (decode (BL.fromStrict (TE.encodeUtf8 def))) } | (_, name, def) <- rows ]

-- Obtener un workflow por ID
getWorkflowById :: DB -> Int -> IO (Maybe Workflow)
getWorkflowById conn wid = do
    rows <- query conn "SELECT name, definition FROM workflows WHERE id = ?" (Only wid) :: IO [(T.Text, T.Text)]
    return $ case rows of
        [(name, def)] -> decode (BL.fromStrict (TE.encodeUtf8 def))
        _ -> Nothing

-- Obtener un workflow por nombre
getWorkflowByName :: DB -> String -> IO (Maybe Workflow)
getWorkflowByName conn name = do
    rows <- query conn "SELECT definition FROM workflows WHERE name = ?" (Only name) :: IO [Only T.Text]
    return $ case rows of
        [Only jsonDef] -> decode (BL.fromStrict (TE.encodeUtf8 jsonDef))
        _ -> Nothing

-- Guardar una tarea en la base de datos
saveTask :: DB -> String -> String -> IO Int
saveTask conn name filePath = do
    execute conn "INSERT INTO tasks (name, file_path) VALUES (?, ?)" (name, filePath)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId)

-- Obtener una tarea por ID
getTaskById :: DB -> Int -> IO (Maybe Task)
getTaskById conn tid = do
    rows <- query conn "SELECT name, file_path FROM tasks WHERE id = ?" (Only tid) :: IO [(T.Text, T.Text)]
    return $ case rows of
        [(name, filePath)] -> Just $ Task { name = T.unpack name, script = Just (T.unpack filePath), command = Nothing, input = [], output = Nothing, depends_on = [] }
        _ -> Nothing