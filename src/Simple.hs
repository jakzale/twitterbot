{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Simple where

import Data.Aeson
import Control.Monad
import Web.Authenticate.OAuth

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics

import Network.HTTP.Conduit

instance FromJSON C.ByteString where
  parseJSON =  liftM C.pack . parseJSON

instance FromJSON Credential where
  parseJSON (Object v) = newCredential <$> v .: "access_token" <*> v .: "access_token_secret"

myoauth :: C.ByteString -> C.ByteString -> OAuth
myoauth key secret = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = key
                              , oauthConsumerSecret = secret
                              }

instance FromJSON OAuth where
  parseJSON (Object v) = myoauth <$> v .: "consumer_key" <*> v .: "consumer_secret"

data Tweet =
  Tweet { text       :: !Text
        , created_at :: !UTCTime
        } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

-- timeline :: String -> IO (Either String [Tweet])
-- timeline name = do
--   req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
--   res <- withManager $ \m -> do
--     signedreq <- signedOAuth 

main :: IO ()
main =
  do
    contents <- L.readFile "config.json"
    putStrLn $ show $ (decode contents :: Maybe Credential)
    putStrLn $ show $ (decode contents :: Maybe OAuth)

