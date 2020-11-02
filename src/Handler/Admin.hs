{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Handler.Guide

getAdminR :: Handler Html
getAdminR = do
    --(_, user) <- requireAuthPair
    (formWidget, enctype) <- generateFormPost $ guideForm Nothing

    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((result, formWidget), enctype) <- runFormPost $ guideForm Nothing

    case result of
        FormSuccess guide -> do
            guideId <- runDB $ insert guide
            setMessage "Guide created successfully"
            redirect $ GuideR guideId

        _ -> msgRedirect "Something went wrong"
   
    defaultLayout $ do
        setTitle "Admin"
        $(widgetFile "admin")

msgRedirect :: Html -> Handler ()
msgRedirect msg = do
    setMessage msg
    redirect AdminR