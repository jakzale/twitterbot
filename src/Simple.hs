{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Simple where

-- Credentials for OAuth

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Web.Authenticate.OAuth
import GHC.Generics

-- loading credentials from JSON
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

instance FromJSON C.ByteString where
  parseJSON v =  liftM C.pack (parseJSON v :: Parser String)

instance FromJSON Credential where
  parseJSON (Object v) = newCredential <$> v .: "key" <*> v .: "secret"

main :: IO ()
main =
  do
    contents <- L.readFile "config.json"
    putStrLn $ show $ (decode contents :: Maybe Credential)

