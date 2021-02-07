{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.YouTube where

import Import

import Data.Aeson.Types
import qualified Data.List as L ((!!))
import Data.Time
import qualified Data.Vector as V (head)
import Network.HTTP.Simple

data YTVideo = YTVideo
    { ytTitle :: Text
    , ytUrl :: Text
    } deriving Show

mkApiUrl :: Text -> String
mkApiUrl ytKey = x ++ unpack ytKey
    where
        x = "https://youtube.googleapis.com/youtube/v3/search?part=snippet&channelId=UCp4IOwajCQ50sEqp-SA4g8g&maxResults=1&order=date&type=video&key="

mkVideoUrl :: String -> Text
mkVideoUrl videoId = "https://www.youtube.com/embed/" <> pack videoId

data YTResponse = YTResponse
    { ytVideoTitle :: String
    , ytVideoId :: String
    } deriving (Show, Read, Generic)

instance ToJSON YTResponse where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON YTResponse where
    parseJSON = withObject "YTResponse" $ \v -> do
        items <- v .: "items"
        let item0 = V.head items

        idObj <- item0 .: "id"
        videoId <- idObj .: "videoId"

        snippet <- item0 .: "snippet"
        title <- snippet .: "title"

        return $ YTResponse title videoId

latestVideoKey :: Text
latestVideoKey = "youtube-latest"

getLatestVideo :: Handler YTVideo
getLatestVideo = do
    now <- liftIO getCurrentTime
    
    mEntCache <- runDB $ getBy $ UniqueCacheId latestVideoKey

    case mEntCache of

        Nothing -> do
            vid <- requestNew $ mkDefault []
            _ <- insertCache now vid
            return vid

        Just entCache
            | now > cacheExpiry (entityVal entCache) -> do
                vid <- requestNew $ mkDefault $ cacheValues $ entityVal entCache
                updateCache (entityKey entCache) now vid
                return vid
            | otherwise -> do
                let vals = cacheValues $ entityVal entCache
                if length vals < 2
                    then do
                        vid <- requestNew $ mkDefault []
                        updateCache (entityKey entCache) now vid
                        return vid
                    else
                        return $ YTVideo (vals L.!! 0) (vals L.!! 1)

    where
        requestNew def = do
            app <- getYesod

            req <- parseRequest $ mkApiUrl $ appYouTubeKey app
            let runReq = fmap getResponseBody (httpJSON req :: Handler (Response YTResponse))

            catch (fmap mkVideo runReq) (\(_ :: HttpException) -> return def)

        mkVideo YTResponse {..} = YTVideo (pack ytVideoTitle) (mkVideoUrl ytVideoId)

        mkDefault :: [Text] -> YTVideo
        mkDefault xs
            | length xs < 2 = YTVideo "" ""
            | otherwise = YTVideo (xs L.!! 0) (xs L.!! 1)

        insertCache now ytVideo = runDB $ insert $ mkCache now ytVideo
        updateCache key now ytVideo = runDB $ replace key $ mkCache now ytVideo
        mkCache now YTVideo {..} = Cache latestVideoKey [ytTitle, ytUrl] $
            addUTCTime (nominalDay / 24) now