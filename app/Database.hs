{-# LANGUAGE OverloadedStrings #-}

module Database 
  ( DB
  , saveWorkflow
  , getWorkflows
  , getWorkflowById
  , saveTask
  , getTaskContentByName
  , getTaskById
  , getTasks
  , taskExists
  , saveExecution
  , getExecutionsByWorkflow
  , getAllExecutions
  , updateExecutionStatus
  , workflowNameExists
  , updateWorkflow
  , deleteWorkflow
  , replaceTaskContent
  , initDB
  ) where

import Control.Monad.Reader

import Database.SQLite.Simple (Connection, execute, query, query_, Only(..), FromRow(..), ToRow(..), SQLData(..), changes, Query)
import Data.String (fromString)
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite.Simple (open, execute_, close, lastInsertRowId)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Types (Workflow(..), WorkflowPatch(..))
import Serialization (encodeJSONText)
import Monad (DatabaseMonad)
import qualified Data.ByteString.Lazy as BL

type DB = Connection

saveWorkflow :: Workflow -> DatabaseMonad Int
saveWorkflow wf = do
    conn <- ask
    let jsonDef = encodeJSONText wf
    liftIO $ do
        execute conn "INSERT INTO workflows (name, definition, status) VALUES (?, ?, ?)" 
            (workflow_name wf, jsonDef, T.pack "pending")
        fromIntegral <$> lastInsertRowId conn

getWorkflows :: DatabaseMonad [(Int, String, T.Text)]
getWorkflows = do
    conn <- ask
    liftIO $ query_ conn "SELECT id, name, definition FROM workflows"

getWorkflowById :: Int -> DatabaseMonad (Maybe (Int, String, T.Text))
getWorkflowById wid = do
    conn <- ask
    liftIO $ do
        rows <- query conn "SELECT id, name, definition FROM workflows WHERE id = ?" (Only wid)
        return $ case rows of
            [row] -> Just row
            _     -> Nothing

saveTask :: String -> BL.ByteString -> DatabaseMonad Int
saveTask name content = do
    conn <- ask
    liftIO $ do
        execute conn "INSERT INTO tasks (name, content) VALUES (?, ?)" (name, content)
        fromIntegral <$> lastInsertRowId conn

getTaskContentByName :: String -> DatabaseMonad (Maybe BL.ByteString)
getTaskContentByName scriptName = do
    conn <- ask
    liftIO $ do
        rows <- query conn "SELECT content FROM tasks WHERE name = ?" (Only scriptName)
        return $ case rows of
            [Only content] -> Just content
            _ -> Nothing

getTaskById :: Int -> DatabaseMonad (Maybe (Int, String))
getTaskById tid = do
    conn <- ask
    liftIO $ do
        rows <- query conn "SELECT id, name FROM tasks WHERE id = ?" (Only tid)
        return $ case rows of
            [(id, name)] -> Just (id, T.unpack name)
            _            -> Nothing

getTasks :: DatabaseMonad [(Int, String)]
getTasks = do
    conn <- ask
    liftIO $ do
        rows <- query_ conn "SELECT id, name FROM tasks"
        return [(id, T.unpack name) | (id, name) <- rows]

taskExists :: String -> DatabaseMonad Bool
taskExists scriptName = do
    conn <- ask
    liftIO $ do
        rows <- query conn "SELECT COUNT(*) FROM tasks WHERE name = ?" (Only scriptName) :: IO [Only Int]
        return $ case rows of
            [Only count] -> count > 0
            _            -> False

saveExecution :: Int -> UTCTime -> DatabaseMonad Int
saveExecution wid timestamp = do
    conn <- ask
    liftIO $ do
        execute conn "INSERT INTO executions (workflow_id, timestamp, status) VALUES (?, ?, 'started')" (wid, timestamp)
        fromIntegral <$> lastInsertRowId conn

updateExecutionStatus :: Int -> String -> DatabaseMonad ()
updateExecutionStatus execId newStatus = do
    conn <- ask
    liftIO $ execute conn "UPDATE executions SET status = ? WHERE id = ?" (newStatus, execId)

getExecutionsByWorkflow :: Int -> DatabaseMonad [(Int, Int, UTCTime, String)]
getExecutionsByWorkflow wid = do
    conn <- ask
    liftIO $ query conn "SELECT id, workflow_id, timestamp, status FROM executions WHERE workflow_id = ? ORDER BY timestamp DESC" (Only wid)

getAllExecutions :: DatabaseMonad [(Int, Int, UTCTime, String)]
getAllExecutions = do
    conn <- ask
    liftIO $ query_ conn "SELECT id, workflow_id, timestamp, status FROM executions ORDER BY timestamp DESC"

workflowNameExists :: String -> DatabaseMonad Bool
workflowNameExists name = do
    conn <- ask
    liftIO $ do
        rows <- query conn "SELECT COUNT(*) FROM workflows WHERE name = ?" (Only name) :: IO [Only Int]
        return $ case rows of
            [Only count] -> count > 0
            _            -> False

updateWorkflow :: Int -> WorkflowPatch -> DatabaseMonad Bool
updateWorkflow wid (WorkflowPatch mName mDef) = do
    conn <- ask
    let namePart = maybe "" (const ", name = ?") mName
    let defPart  = maybe "" (const ", definition = ?") mDef
    let sqlStr = "UPDATE workflows SET status = 'pending'" ++ namePart ++ defPart ++ " WHERE id = ?"
        sql = fromString sqlStr
        params = case (mName, mDef) of
            (Just n, Just d) -> [toField n, toField (encodeJSONText d), toField wid]
            (Just n, Nothing) -> [toField n, toField wid]
            (Nothing, Just d) -> [toField (encodeJSONText d), toField wid]
            _ -> [toField wid]
    n <- liftIO $ execute conn sql params >> changes conn
    return (n > 0)

deleteWorkflow :: Int -> DatabaseMonad Bool
deleteWorkflow wid = do
    conn <- ask
    liftIO $ execute conn "DELETE FROM workflows WHERE id = ?" (Only wid) >> (>0) <$> changes conn

replaceTaskContent :: String -> BL.ByteString -> DatabaseMonad ()
replaceTaskContent name newContent = do
  conn <- ask
  liftIO $ execute conn "UPDATE tasks SET content = ? WHERE name = ?" (newContent, name)


initDB :: IO ()
initDB = do
  conn <- open "workflows2.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS workflows (id INTEGER PRIMARY KEY, name TEXT UNIQUE, definition TEXT, status TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS executions (id INTEGER PRIMARY KEY, workflow_id INTEGER, timestamp TEXT, status TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id INTEGER PRIMARY KEY, name TEXT UNIQUE, content BLOB)"
  close conn
