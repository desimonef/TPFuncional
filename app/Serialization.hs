{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Serialization 
  ( encodeJSON
  , decodeJSON
  , encodeJSONText
  , decodeJSONText
  , jsonErrorBody
  ) where

import Data.Aeson (ToJSON(..), FromJSON, encode, decode, parseJSON, withObject, object, (.=), (.:?), Value(..), Object)
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Types (Workflow(..), Task(..), TaskInput(..), TaskOutput(..), IdResponse(..),
              WorkflowResponse(..), ExecutionResponse(..), ExecutionRecord(..),
              RetryPolicy(..), FailStrategy(..), WorkflowPatch(..))

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = encode

-- Deserializar desde JSON
decodeJSON :: FromJSON a => ByteString -> Maybe a
decodeJSON = decode

-- Serializar JSON en formato `Text` para bases de datos
encodeJSONText :: ToJSON a => a -> T.Text
encodeJSONText = decodeUtf8 . toStrict . encodeJSON

-- Deserializar JSON desde `Text`
decodeJSONText :: FromJSON a => T.Text -> Maybe a
decodeJSONText = decodeJSON . fromStrict . encodeUtf8

-- Convertir String a cuerpo de error HTTP (lazy ByteString)
jsonErrorBody :: String -> ByteString
jsonErrorBody = fromStrict . encodeUtf8 . T.pack

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

instance ToJSON TaskInput where
    toJSON (FileInput f) = object ["file" .= f]
    toJSON (VarInput v)  = object ["var" .= v]

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

instance FromJSON WorkflowPatch
