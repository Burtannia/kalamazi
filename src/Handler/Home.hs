{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Home where

import Import
import Data.Aeson.Types
import Handler.Images
import Handler.Guide
import Handler.AdminTools
import Network.HTTP.Simple
import qualified Data.Vector as V (head)

mkYouTubeUrl :: Text -> String
mkYouTubeUrl ytKey = x ++ unpack ytKey
    where
        x = "https://youtube.googleapis.com/youtube/v3/search?part=snippet&channelId=UCp4IOwajCQ50sEqp-SA4g8g&maxResults=1&order=date&type=video&key="
        
getHomeR :: Handler Html
getHomeR = do
    app <- getYesod
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser

    req <- parseRequest $ mkYouTubeUrl $ appYouTubeKey app
    ytData <- fmap getResponseBody (httpJSON req :: Handler (Response YTResponse))
    
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        getImageManager
                        ggManager
                        genNewGuide
                        Nothing
                else
                    Nothing
        $(widgetFile "homepage")        

postHomeR :: Handler Html
postHomeR = do
    app <- getYesod
    muser <- maybeAuth
    let isAdmin = maybe False (userIsAdmin . entityVal) muser

    req <- parseRequest $ mkYouTubeUrl $ appYouTubeKey app
    ytData <- fmap getResponseBody (httpJSON req :: Handler (Response YTResponse))

    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        let madminTools =
                if isAdmin then
                    Just $ mkAdminTools $ 
                        AdminTools
                        postImageManager
                        ggManager
                        runNewGuide
                        Nothing
                else
                    Nothing
        $(widgetFile "homepage")

mkVideoUrl :: String -> String
mkVideoUrl videoId = "https://www.youtube.com/embed/" ++ videoId

data YTResponse = YTResponse
    { ytVideoId :: String
    , ytVideoTitle :: String
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

        return $ YTResponse videoId title 