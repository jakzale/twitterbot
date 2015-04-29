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

data JSONCredentials =
  JSONCredentials { key    :: !String
                  , secret :: !String
                  }
  deriving (Show, Generic)

g :: JSONCredentials -> Credential
g json = newCredential (key json) (secret json)

main :: IO ()
main = putStrLn "Hello, world!"
