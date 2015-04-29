{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Simple where

-- Credentials for OAuth

import Data.Aeson
import Control.Monad
import Web.Authenticate.OAuth
import GHC.Generics


main :: IO ()
main = putStrLn "Hello, world!"
