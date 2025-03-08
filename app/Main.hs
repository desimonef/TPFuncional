{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Database (DB, initDB, saveWorkflow)
import API(runServer)


main :: IO ()
main = do
    db <- initDB
    runServer db
