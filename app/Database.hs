{-# LANGUAGE OverloadedStrings #-}

module Database (DB, initDB, saveWorkflow, getWorkflows, getWorkflowById, getWorkflowByName, saveTask, getTaskById, getTasks, taskExists, saveExecution, getExecutionsByWorkflow, getAllExecutions, updateExecutionStatus) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time.Clock (UTCTime)
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
    execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE)"
    execute_ conn "CREATE TABLE IF NOT EXISTS executions (id INTEGER PRIMARY KEY AUTOINCREMENT, workflow_id INTEGER NOT NULL, timestamp TEXT NOT NULL, status TEXT CHECK(status IN ('started', 'completed', 'failed')) NOT NULL DEFAULT 'started', FOREIGN KEY(workflow_id) REFERENCES workflows(id))"
    return conn

-- Guardar un workflow en la base de datos
saveWorkflow :: DB -> Workflow -> IO Int
saveWorkflow conn wf = do
    let jsonDef = TE.decodeUtf8 . BL.toStrict $ encode wf
    execute conn "INSERT INTO workflows (name, definition, status) VALUES (?, ?, ?)" 
        (workflow_name wf, jsonDef, T.pack "pending")
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId)

-- Obtener todos los workflows con ID y definición en texto plano
getWorkflows :: DB -> IO [(Int, String, T.Text)]
getWorkflows conn = do
    query_ conn "SELECT id, name, definition FROM workflows" :: IO [(Int, String, T.Text)]

-- Obtener un workflow por ID y devolver su ID y definición en texto plano
getWorkflowById :: DB -> Int -> IO (Maybe (Int, String, T.Text))
getWorkflowById conn wid = do
    rows <- query conn "SELECT id, name, definition FROM workflows WHERE id = ?" (Only wid) :: IO [(Int, String, T.Text)]
    return $ case rows of
        [(id, name, def)] -> Just (id, name, def)
        _ -> Nothing

-- Obtener un workflow por nombre
getWorkflowByName :: DB -> String -> IO (Maybe (Int, Workflow))
getWorkflowByName conn name = do
    rows <- query conn "SELECT id, definition FROM workflows WHERE name = ?" (Only name) :: IO [(Int, T.Text)]
    return $ case rows of
        [(id, jsonDef)] -> Just (id, Workflow { workflow_name = name, tasks = maybe [] (const []) (decode (BL.fromStrict (TE.encodeUtf8 jsonDef)) :: Maybe [Task]) })
        _ -> Nothing

-- Guardar una tarea en la base de datos
saveTask :: DB -> String -> IO Int
saveTask conn name = do
    execute conn "INSERT INTO tasks (name) VALUES (?)" (Only name)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId)

-- Obtener una tarea por ID
getTaskById :: DB -> Int -> IO (Maybe (Int, String))
getTaskById conn tid = do
    rows <- query conn "SELECT id, name FROM tasks WHERE id = ?" (Only tid) :: IO [(Int, T.Text)]
    return $ case rows of
        [(id, name)] -> Just (id, T.unpack name)
        _ -> Nothing

-- Obtener todas las tareas con ID
getTasks :: DB -> IO [(Int, String)]
getTasks conn = do
    rows <- query_ conn "SELECT id, name FROM tasks" :: IO [(Int, T.Text)]
    return [(id, T.unpack name) | (id, name) <- rows]

-- Verificar si una tarea existe en la base de datos
taskExists :: DB -> String -> IO Bool
taskExists conn scriptName = do
    putStrLn $ "Checking existence of task: " ++ scriptName  -- Debugging
    rows <- query conn "SELECT COUNT(*) FROM tasks WHERE name = ?" (Only scriptName) :: IO [Only Int]
    return $ case rows of
        [Only count] -> count > 0
        _ -> False

saveExecution :: DB -> Int -> UTCTime -> IO Int
saveExecution conn wid timestamp = do
    execute conn "INSERT INTO executions (workflow_id, timestamp, status) VALUES (?, ?, 'started')" (wid, timestamp)
    [Only lastId] <- query_ conn "SELECT last_insert_rowid()" :: IO [Only Int]
    return lastId

updateExecutionStatus :: DB -> Int -> String -> IO ()
updateExecutionStatus conn execId newStatus = do
    execute conn "UPDATE executions SET status = ? WHERE id = ?" (newStatus, execId)

getExecutionsByWorkflow :: DB -> Int -> IO [(Int, Int, UTCTime, String)]
getExecutionsByWorkflow conn wid = do
    query conn "SELECT id, workflow_id, timestamp, status FROM executions WHERE workflow_id = ? ORDER BY timestamp DESC"
        (Only wid)

getAllExecutions :: DB -> IO [(Int, Int, UTCTime, String)]
getAllExecutions conn = do
    query_ conn "SELECT id, workflow_id, timestamp, status FROM executions ORDER BY timestamp DESC"

