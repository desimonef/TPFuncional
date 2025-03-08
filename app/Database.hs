{-# LANGUAGE OverloadedStrings #-}

module Database (DB, initDB, saveWorkflow, getWorkflows, getWorkflowById, getWorkflowByName, saveTask, getTaskById, getTasks, taskExists) where

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

-- Obtener todos los workflows con ID
getWorkflows :: DB -> IO [(Int, Workflow)]
getWorkflows conn = do
    rows <- query_ conn "SELECT id, name, definition FROM workflows" :: IO [(Int, T.Text, T.Text)]
    return [ (id, Workflow { workflow_name = T.unpack name, tasks = maybe [] (const []) (decode (BL.fromStrict (TE.encodeUtf8 def)) :: Maybe [Task]) }) | (id, name, def) <- rows ]

-- Obtener un workflow por ID
getWorkflowById :: DB -> Int -> IO (Maybe (Int, Workflow))
getWorkflowById conn wid = do
    rows <- query conn "SELECT id, name, definition FROM workflows WHERE id = ?" (Only wid) :: IO [(Int, T.Text, T.Text)]
    return $ case rows of
        [(id, name, def)] -> Just (id, Workflow { workflow_name = T.unpack name, tasks = maybe [] (const []) (decode (BL.fromStrict (TE.encodeUtf8 def)) :: Maybe [Task]) })
        _ -> Nothing

-- Obtener un workflow por nombre
getWorkflowByName :: DB -> String -> IO (Maybe (Int, Workflow))
getWorkflowByName conn name = do
    rows <- query conn "SELECT id, definition FROM workflows WHERE name = ?" (Only name) :: IO [(Int, T.Text)]
    return $ case rows of
        [(id, jsonDef)] -> Just (id, Workflow { workflow_name = name, tasks = maybe [] (const []) (decode (BL.fromStrict (TE.encodeUtf8 jsonDef)) :: Maybe [Task]) })
        _ -> Nothing

-- Guardar una tarea en la base de datos
saveTask :: DB -> String -> String -> IO Int
saveTask conn name filePath = do
    execute conn "INSERT INTO tasks (name, file_path) VALUES (?, ?)" (name, filePath)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId)

-- Obtener una tarea por ID
getTaskById :: DB -> Int -> IO (Maybe (Int, String))
getTaskById conn tid = do
    rows <- query conn "SELECT id, name, file_path FROM tasks WHERE id = ?" (Only tid) :: IO [(Int, T.Text, T.Text)]
    return $ case rows of
        [(id, name, _)] -> Just (id, T.unpack name)
        _ -> Nothing

-- Obtener todas las tareas con ID
getTasks :: DB -> IO [(Int, String)]
getTasks conn = do
    rows <- query_ conn "SELECT id, name, file_path FROM tasks" :: IO [(Int, T.Text, T.Text)]
    return [(id, T.unpack name) | (id, name, _) <- rows]

-- Verificar si una tarea existe en la base de datos
taskExists :: DB -> String -> IO Bool
taskExists conn scriptName = do
    putStrLn $ "Checking existence of task: " ++ scriptName  -- Debugging
    rows <- query conn "SELECT COUNT(*) FROM tasks WHERE file_path LIKE ?" (Only scriptName) :: IO [Only Int]
    return $ case rows of
        [Only count] -> count > 0
        _ -> False