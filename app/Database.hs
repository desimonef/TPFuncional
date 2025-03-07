{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database (DB, initDB, saveWorkflow, getWorkflow) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (bracket)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as B

-- Tipo de conexión a la base de datos
type DB = Connection

-- Estructura para mapear filas de la base de datos
data TaskRecord = TaskRecord String Bool deriving (Show)

instance FromRow TaskRecord where
    fromRow = TaskRecord <$> field <*> field

-- Inicializa la base de datos y crea la tabla si no existe
initDB :: IO DB
initDB = do
    conn <- open "workflow.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS workflows (id SERIAL PRIMARY KEY, name TEXT UNIQUE NOT NULL, definition JSONB NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
    -- execute_ conn "CREATE TABLE IF NOT EXISTS tasks (id SERIAL PRIMARY KEY, workflow_id INT REFERENCES workflows(id) ON DELETE CASCADE, name TEXT NOT NULL, command TEXT NOT NULL, depends_on TEXT[], created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
    return conn

-- Guardar un nuevo workflow en la base de datos
saveWorkflow :: DB -> String -> B.ByteString -> IO Int
saveWorkflow conn name definition = do
    execute conn "INSERT INTO workflows (name, definition) VALUES (?, ?)" (name, definition)
    rowId <- lastInsertRowId conn
    return (fromIntegral rowId :: Int) -- Convertimos directamente a Int

-- Obtener un workflow de la base de datos
getWorkflow :: DB -> Int -> IO (Maybe B.ByteString)
getWorkflow conn workflowId = do
    results <- query conn "SELECT definition FROM workflows WHERE id = ?" (Only workflowId) :: IO [Only String]
    return $ case results of
        [Only definition] -> Just (B.pack definition)
        _ -> Nothing

