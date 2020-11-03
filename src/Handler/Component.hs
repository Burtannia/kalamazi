{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Component where

import Import
import Handler.Images
import qualified Data.Text as T (append)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

import Model.Guide

displayComponent :: Component -> Widget
displayComponent (CMarkup markupId) = do
    markup <- handlerToWidget $ runDB $ getJust markupId
    [whamlet|
        <div .component .compMarkup>
            #{markupComponentContent markup}
    |]

displayComponent (CImage imageId) = do
    image <- handlerToWidget $ runDB $ getJust imageId
    [whamlet|
        <div .component .compImage>
            <img src=@{ImagesR $ mkImageUrl image} alt=#{imageName image}>
    |]

displayComponent (CIframe link) =
    [whamlet|
        <div .component .compIframe>
            <iframe src=#{link}>
    |]

displayComponent (CToggle toggle) = displayToggle toggle

displayComponent (CGrid opts rs) = -- currently ignoring grid opts
    [whamlet|
        <div .compGrid>
            $forall r <- rs
                <div .row>
                    $forall comp <- r
                        <div .col-auto>
                            ^{displayComponent comp}  
    |]

displayToggle :: ToggleGroup -> Widget
displayToggle (ToggleTexts sc toggles) = toggleWidget parts
    where
        ids = [1..length toggles]
        parts = map mkPart $ zip ids toggles
        mkPart (n, (sel, cont)) = (n, mkWidget sel, cont)
        mkWidget s = -- maybe add space char as css ::after
            [whamlet|
                <p>#{s}
            |]
displayToggle (ToggleImages borderImgId toggles) = toggleWidget parts
    where
        ids = [1..length toggles]
        parts = map mkPart $ zip ids toggles
        mkPart (n, (sel, cont)) = (n, mkWidget sel, cont)
        mkWidget imgId = do
            img <- handlerToWidget $ runDB $ getJust imgId
            [whamlet|
                <img src=@{ImagesR $ mkImageUrl img}>
            |]

toggleWidget :: [(Int, Widget, MarkupComponentId)] -> Widget
toggleWidget parts = do
    [whamlet|
        <div .component .compToggle>
            <div .toggleSelectorsContainer>
                $forall (toggleId, selectorWidget, _) <- parts
                    <div .toggleSelector  data-toggle-id=#{toggleId}>
                        ^{selectorWidget}

            <div .toggleContentContainer>
                $forall (toggleId, _, contentId) <- parts
                    <div .toggleContent data-toggle-id=#{toggleId}>
                        ^{displayComponent $ CMarkup contentId}
    |]