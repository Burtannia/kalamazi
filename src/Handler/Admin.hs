{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Handler.Image
import Data.Time.Format.ISO8601

getAdminR :: Handler Html
getAdminR = do
    --(_, user) <- requireAuthPair
    (formWidget, enctype) <- generateFormPost uploadForm
    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((result, formWidget), enctype) <- runFormPost uploadForm
    case result of
        FormSuccess uploadImg -> do
            app <- getYesod

            let file = iuFile uploadImg
                mExt = parseExt $ fileContentType file
                uuid = iso8601Show $ iuTime uploadImg
                dir = appImageDir $ appSettings app
            
            case mExt of
                Nothing -> msgRedirect "Unsupported file type"
                Just ext -> do
                    let newImg = Image (pack uuid) (iuName uploadImg) ext (iuTime uploadImg)
                    liftIO $ fileMove (iuFile uploadImg) (mkImagePath dir newImg)
                    _ <- runDB $ insert newImg
                    msgRedirect "Image uploaded successfully"

        _ -> msgRedirect "Something went wrong"
    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")

msgRedirect :: Html -> Handler ()
msgRedirect msg = do
    setMessage msg
    redirect AdminR