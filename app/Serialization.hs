{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Serialization 
  ( encodeJSON
  , decodeJSON
  , encodeJSONText
  , decodeJSONText
  , extractJSONResult
  , jsonErrorBody
  ) where

import Data.Aeson (ToJSON, FromJSON, encode, decode, parseJSON, withObject, (.:), (.:?), Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Types (Workflow(..), Task(..), TaskInput(..), TaskOutput(..), IdResponse(..),
              WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..),
              RetryPolicy(..), FailStrategy(..))

-- Serializar a JSON
encodeJSON :: ToJSON a => a -> BL.ByteString
encodeJSON = encode

-- Deserializar desde JSON
decodeJSON :: FromJSON a => BL.ByteString -> Maybe a
decodeJSON = decode

-- Serializar JSON en formato `Text` para bases de datos
encodeJSONText :: ToJSON a => a -> T.Text
encodeJSONText = TE.decodeUtf8 . BL.toStrict . encodeJSON

-- Deserializar JSON desde `Text`
decodeJSONText :: FromJSON a => T.Text -> Maybe a
decodeJSONText = decodeJSON . BL.fromStrict . TE.encodeUtf8

-- Convertir String a cuerpo de error HTTP (lazy ByteString)
jsonErrorBody :: String -> BL.ByteString
jsonErrorBody = BL.fromStrict . TE.encodeUtf8 . T.pack

-- Extrae el resultado de un JSON con la clave "result"
extractJSONResult :: String -> Maybe String
extractJSONResult output = do
    let jsonBytes = B.pack output 
    jsonObject <- decodeJSON jsonBytes :: Maybe Object
    value <- KM.lookup "result" jsonObject
    pure (valueToString value)

-- Convierte un valor JSON a String
valueToString :: Value -> String
valueToString (String s)  = T.unpack s
valueToString (Number n)  = show n
valueToString (Bool b)    = show b
valueToString Null        = "null"
valueToString (Array a)   = show a
valueToString (Object o)  = show o

-- Instancias JSON
instance ToJSON Workflow
instance FromJSON Workflow

instance ToJSON Task
instance FromJSON Task

instance ToJSON TaskInput
instance FromJSON TaskInput where
    parseJSON = withObject "TaskInput" $ \o -> do
        mFile <- o .:? "file"
        mVar  <- o .:? "var"
        case (mFile, mVar) of
            (Just file, Nothing) -> return $ FileInput file
            (Nothing, Just var)  -> return $ VarInput var
            _ -> fail "TaskInput debe contener exactamente una clave 'file' o 'var'"

instance ToJSON TaskOutput
instance FromJSON TaskOutput

instance ToJSON IdResponse
instance FromJSON IdResponse

instance ToJSON WorkflowResponse
instance FromJSON WorkflowResponse

instance ToJSON ExecutionResponse
instance FromJSON ExecutionResponse

instance ToJSON ExecutionRecord
instance FromJSON ExecutionRecord

instance ToJSON RetryPolicy
instance FromJSON RetryPolicy

instance ToJSON FailStrategy
instance FromJSON FailStrategy
