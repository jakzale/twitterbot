{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Simple where

-- Credentials for OAuth

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Web.Authenticate.OAuth
import GHC.Generics

-- loading credentials from JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

data JSONCredentials =
  JSONCredentials { key    :: !String
                  , secret :: !String
                  }
  deriving (Show, Generic)

instance FromJSON JSONCredentials

g :: JSONCredentials -> Credential
g json = newCredential (fmap C.pack key json) (fmap C.pack secret json)

main :: IO ()
main =
  do
    contents <- L.readFile "config.json"
    putStrLn $ show $ liftM g $ (decode contents :: Maybe JSONCredentials)

