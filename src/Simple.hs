{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Simple where

import Data.Aeson
import Data.Maybe
import Control.Monad
import Web.Authenticate.OAuth

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics

import Network.HTTP.Conduit
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Control.Applicative

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

timeline' :: String -> Timeline
timeline' name = do
  -- L.readFile is in IO, so we need to liftIO it
  contents <- liftIO $ L.readFile "config.json"
  let myoauth = (decode contents :: Maybe OAuth)
  let mycred  = (decode contents :: Maybe Credential)
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  res <- withManager $ \m -> do
    signedreq <- signOAuth (fromJust myoauth) (fromJust mycred) req
    httpLbs signedreq m
  -- eitherDecode returns Either, hoistEither lifts it to EitherT
  hoistEither $ eitherDecode $ responseBody res

timeline :: String -> IO (Either String [Tweet])
timeline name = do
  -- Loading Credentials
  contents <- L.readFile "config.json"
  let myoauth = (decode contents :: Maybe OAuth)
  let mycred = (decode contents :: Maybe Credential)
  -- Creating the Request
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  -- Lets get the response
  -- TODO: figure out this
  res <- withManager $ \m -> do
    signedreq <- signOAuth (fromJust myoauth) (fromJust mycred) req
    httpLbs signedreq m
  -- Decode the response body
  return $ eitherDecode $ responseBody res

main :: IO ()
main = eitherT onFailure onSuccess (timeline' "Hackage")
  where
    onSuccess :: [Tweet] -> IO ()
    onSuccess ts = mapM_ print $ take 5 ts
    onFailure :: String -> IO ()
    onFailure = putStrLn
