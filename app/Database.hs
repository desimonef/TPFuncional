{-# LANGUAGE OverloadedStrings #-}

module Database 
  ( DB
  , saveWorkflow
  , getWorkflows
  , getWorkflowById
  , saveTask
  , getTaskById
  , getTasks
  , taskExists
  , saveExecution
  , getExecutionsByWorkflow
  , getAllExecutions
  , updateExecutionStatus
  ) where

import Control.Monad.Reader

import Database.SQLite.Simple

import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Types (Workflow(..),)
import Serialization (encodeJSONText)
import Monad (DatabaseMonad)

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

saveTask :: String -> DatabaseMonad Int
saveTask name = do
    conn <- ask
    liftIO $ do
        execute conn "INSERT INTO tasks (name) VALUES (?)" (Only name)
        fromIntegral <$> lastInsertRowId conn

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
