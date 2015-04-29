{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Simple where

import Data.Aeson
import Control.Monad
import Web.Authenticate.OAuth

-- loading credentials from JSON
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

instance FromJSON C.ByteString where
  parseJSON =  liftM C.pack . parseJSON

instance FromJSON Credential where
  parseJSON (Object v) = newCredential <$> v .: "key" <*> v .: "secret"

main :: IO ()
main =
  do
    contents <- L.readFile "config.json"
    putStrLn $ show $ (decode contents :: Maybe Credential)

