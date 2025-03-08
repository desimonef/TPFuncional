{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Database (DB, initDB, saveWorkflow)
import Workflows (Workflow(..), executeWorkflow)
import Data.Aeson (decode, eitherDecode)


main :: IO ()
main = do
    db <- initDB
    contents <- B.readFile "workflow.json"

    case eitherDecode contents of
        Left err -> do
            putStrLn "Error en la lectura:"
            putStrLn err 
        Right wf -> do
            putStrLn $ "Ejecutando workflow: " ++ workflow_name wf
            executeWorkflow db wf
