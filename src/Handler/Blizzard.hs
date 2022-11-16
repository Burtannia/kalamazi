{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Blizzard where

import Import

import Data.Aeson.Types
import qualified Data.List as L ((!!))
import Data.Time
import qualified Data.Vector as V (head)
import Network.HTTP.Simple
import Model.Core (Unique(UniqueCacheId))

data AccessTokenResp = ATR
    { atrToken :: Text
    , atrType :: Text
    , atrExpires :: Int
    } deriving (Show, Read, Generic)

instance ToJSON AccessTokenResp where
    toEncoding = genericToEncoding defaultOptions   

instance FromJSON AccessTokenResp where
    parseJSON = withObject "ATR" $ \v -> do
        atrToken <- v .: "access_token"
        atrType <- v .: "token_type"
        atrExpires <- v .: "expires_in"
        return $ ATR {..}

accessTokenKey :: Text
accessTokenKey = "blizzard-access-token"

getAccessToken :: Handler (Maybe Text)
getAccessToken = do
    now <- liftIO getCurrentTime

    -- mEntCache <- runDB $ getBy $ UniqueCacheId accessTokenKey

    -- case mEntCache of
    --     Nothing -> do
    --         -- fetch access token
    --         -- if successful then store it
    --     Just entCache -> do
    --         -- check if valid
    --         -- if so then return
    --         -- otherwise attempt to fetch new token

    token <- requestNew Nothing
    liftIO $ print token
    return token

    where
        requestNew def = do
            app <- getYesod
            let BlizzardCreds {..} = appBlizzardCreds $ appSettings app
                -- reqObject = object [ "grant_type" .= ("client_credentials" :: Text) ]
                req = urlEncodedBody [("grant_type", "client_credentials")]
                    $ applyBasicAuth (encodeUtf8 blizzClientId) (encodeUtf8 blizzClientSecret)
                    $ parseRequest_ "POST https://oauth.battle.net/token"
                runReq = getResponseBody <$> (httpJSON req :: Handler (Response AccessTokenResp))

            catch (Just . atrToken <$> runReq) (\(_ :: HttpException) -> return def)


data SearchReq = SearchSpell Text | SearchItem Text
    deriving (Show, Eq)

searchType :: SearchReq -> Text
searchType (SearchSpell _) = "spell"
searchType (SearchItem _)  = "item"

searchTerm :: SearchReq -> Text
searchTerm (SearchSpell x) = x
searchTerm (SearchItem x)  = x

mkSearchUrl :: SearchReq -> Request
mkSearchUrl req = "eu.api.blizzard.com/data/wow/search/"
    <> searchType req
    <> params
    where
        params = "?namespace=static-eu" <> termParam
        termParam = "&name.en_US=" <> searchTerm req

mkMediaUrl :: SearchReq -> Request
mkMediaUrl req = "eu.api.blizzard.com/data/wow/media/"
    <> urlSuffix
    <> "?namespace=static-eu"
    where
        urlSuffix = searchType req <> "/" <> searchTerm req

newtype SearchResp = SearchResp [Int]
    deriving (Show, Eq)

instance ToJSON SearchRep where
    toEncoding = genericToEncoding defaultOptions   

instance FromJSON SearchResp where
    parseJSON = withObject "SearchResp" $ \v -> do
        results <- v .: "results"
        ids <- forM results $ \o -> do
            theData <- o .: "data"
            media <- theData .: "media"
            media .: "id"
        return $ SearchResp ids

data MediaResp = MediaResp
    { mediaId  :: Int
    , mediaUrl :: Maybe Text
    } deriving (Show, Eq)

instance ToJSON MediaResp where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON MediaResp where
    parseJSON = withObject "Icon" $ \v -> do
        mediaId <- v.: "id"
        assets <- v .: "assets"
        links <- forM assets $ \o -> do
            k <- o .: "key"
            v <- o .: "value"
            return (k, v)
        let mediaUrl = find ((==) "icon" . fst) links
        return $ MediaResp {..}

data Icon = Icon
    { iconName :: Text
    , iconMediaId :: Int
    , iconUrl :: Text
    } deriving (Show, Eq)

searchIcon :: Text -> Handler [Icon]
searchIcon query = do

-- getLatestVideo :: Handler YTVideo
-- getLatestVideo = do
--     now <- liftIO getCurrentTime
    
--     let returnCache ec = do
--             let vals = cacheValues $ entityVal ec
--             if length vals < 2
--                 then do
--                     vid <- requestNew $ mkDefault []
--                     updateCache (entityKey ec) now vid
--                     return vid
--                 else
--                     return $ YTVideo (vals L.!! 0) (vals L.!! 1)

--     mEntCache <- runDB $ getBy $ UniqueCacheId latestVideoKey

--     case mEntCache of

--         Nothing -> do
--             vid <- requestNew $ mkDefault []
--             _ <- insertCache now vid
--             return vid

--         Just entCache
--             | now > cacheExpiry (entityVal entCache) -> do
--                 _ <- async $ do
--                     vid <- requestNew $ mkDefault $ cacheValues $ entityVal entCache
--                     updateCache (entityKey entCache) now vid
--                 returnCache entCache
--             | otherwise -> returnCache entCache

--     where
--         requestNew defs = do
--             app <- getYesod

--             req <- parseRequest $ mkApiUrl $ appYouTubeKey app
--             let runReq = fmap getResponseBody (httpJSON req :: Handler (Response YTResponse))

--             catch (fmap mkVideo runReq) (\(_ :: HttpException) -> return defs)

--         mkVideo YTResponse {..} = YTVideo (pack ytVideoTitle) (mkVideoUrl ytVideoId)

--         mkDefault :: [Text] -> YTVideo
--         mkDefault xs
--             | length xs < 2 = YTVideo "" ""
--             | otherwise = YTVideo (xs L.!! 0) (xs L.!! 1)

--         insertCache now ytVideo = runDB $ insert $ mkCache now ytVideo
--         updateCache key now ytVideo = runDB $ replace key $ mkCache now ytVideo
--         mkCache now YTVideo {..} = Cache latestVideoKey [ytTitle, ytUrl] $
--             addUTCTime (nominalDay / 24) now