{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Database (DB, initDB, saveWorkflow)
import Workflows (Workflow(..), getExecutionOrder, executeTasks)
import Data.Aeson (decode)

main :: IO ()
main = do
    db <- initDB
    contents <- B.readFile "workflow.json"
    case decode contents of
        Just wf -> do
            putStrLn $ "Ejecutando workflow: " ++ workflow_name wf
            workflowId <- saveWorkflow db (workflow_name wf) contents
            let executionLevels = getExecutionOrder (tasks wf)
            executeTasks db executionLevels
        Nothing -> putStrLn "Error en la lectura"
