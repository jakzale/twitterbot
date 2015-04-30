{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Simple where

import Data.Aeson
import Web.Authenticate.OAuth
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans

instance FromJSON C.ByteString where
  parseJSON =  liftM C.pack . parseJSON

instance FromJSON Credential where
  parseJSON (Object v) = newCredential <$> v .: "access_token" <*> v .: "access_token_secret"

mkOAuth :: C.ByteString -> C.ByteString -> OAuth
mkOAuth key secret = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = key
                              , oauthConsumerSecret = secret
                              }

instance FromJSON OAuth where
  parseJSON (Object v) = mkOAuth <$> v .: "consumer_key" <*> v .: "consumer_secret"

data Tweet =
  Tweet { text       :: !Text
        -- , created_at :: !UTCTime
        } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

type Timeline = EitherT String IO [Tweet]

timeline :: String -> Timeline
timeline name = do
  -- L.readFile is in IO, so we need to liftIO it
  contents <- liftIO $ L.readFile "config.json"
  -- hoistEither lifts Either to EitherT
  myoauth <- hoistEither $ eitherDecode contents
  mycred  <- hoistEither $ eitherDecode contents
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  res <- withManager $ \m -> do
    signedreq <- signOAuth myoauth mycred req
    httpLbs signedreq m
  hoistEither $ eitherDecode $ responseBody res

main :: IO ()
main = eitherT onFailure onSuccess (timeline "Hackage")
  where
    onSuccess :: [Tweet] -> IO ()
    onSuccess ts = mapM_ print $ take 5 ts
    onFailure :: String -> IO ()
    onFailure = putStrLn
