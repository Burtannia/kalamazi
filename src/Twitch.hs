{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Twitch where

import Import
import Data.Aeson
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE
import           Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import Data.Time
import System.Environment

data StreamsResponse = StreamsResponse { srData :: [Value] }
  deriving (Show)

instance FromJSON StreamsResponse where
  parseJSON = withObject "StreamsResponse" $ \o ->
    StreamsResponse <$> o .: "data"

data TokenResponse = TokenResponse
  { accessToken :: Text
  , expiresIn   :: Int
  } deriving Show

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o ->
    TokenResponse <$> o .: "access_token"
                  <*> o .: "expires_in"

getNewToken :: Handler TwitchToken
getNewToken = do
    app <- getYesod

    today <- utctDay <$> liftIO getCurrentTime
    let twitchCreds = appTwitchCreds app
    liftIO $ runReq defaultHttpConfig $ do
        let body =
              "client_id"     =: (twitchClientId twitchCreds) <>
              "client_secret" =: (twitchClientSecret twitchCreds) <>
              "grant_type"    =: ("client_credentials" :: Text)
        r <- req
               Req.POST
               (https "id.twitch.tv" /: "oauth2" /: "token")
               (ReqBodyUrlEnc body)
               jsonResponse
               mempty
        let tokenResp = Req.responseBody r
            daysLeft = (expiresIn tokenResp) `div` (24 * 60 * 60)
            expires = addDays (toInteger daysLeft) today
        pure (TwitchToken (accessToken tokenResp) expires)

getTwitchToken :: Handler TwitchToken
getTwitchToken = do
    app <- getYesod
    let tokenRef = appTwitchToken app
    mbToken <- liftIO $ readIORef tokenRef
    today <- utctDay <$> liftIO getCurrentTime
    (t, isNew) <- case mbToken of
        Nothing -> do
            newToken <- getNewToken
            pure (newToken, True)
        Just t
            | expires t <= today -> do
                newToken <- getNewToken
                pure (newToken, True)
            | otherwise -> pure (t, False)

    when isNew $ atomicWriteIORef tokenRef (Just t)
    pure t

mainIsLive :: Handler Bool
mainIsLive = do
    app <- getYesod
    let twitchCreds = appTwitchCreds app
    TwitchToken { token } <- getTwitchToken
    runReq defaultHttpConfig $ do
        let authHeader = header "Authorization" (B.concat ["Bearer ", TE.encodeUtf8 token])
            clientHeader = header "Client-Id" (TE.encodeUtf8 (twitchClientId twitchCreds))
        r <- req
            Req.GET
            (https "api.twitch.tv" /: "helix" /: "streams")
            NoReqBody
            jsonResponse
            (authHeader <> clientHeader <> ("user_login" =: ("kalamazi" :: Text)))
        let StreamsResponse xs = Req.responseBody r
        pure (not (Prelude.null xs))
