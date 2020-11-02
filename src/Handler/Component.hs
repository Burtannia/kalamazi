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

displayComponent (CBackgroundImg imageId) = undefined

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
displayToggle (ToggleTexts sc toggles) = toggleWidget selectors contents
    where
        ids = [1..length toggles]
        selectors = zip ids $ map (mkWidget . fst) toggles
        contents = zip ids $ map snd toggles
        mkWidget s = -- maybe add space char as css ::after
            [whamlet|
                <p>#{s}
            |]
displayToggle (ToggleImages borderImgId toggles) = toggleWidget selectors contents
    where
        ids = [1..length toggles]
        selectors = zip ids $ map (mkWidget . fst) toggles
        contents = zip ids $ map snd toggles
        mkWidget imgId = do
            img <- handlerToWidget $ runDB $ getJust imgId
            [whamlet|
                <img src=@{ImagesR $ mkImageUrl img}>
            |]

toggleWidget :: [(Int, Widget)] -> [(Int, [Component])] -> Widget
toggleWidget selectors contents =
    [whamlet|
        <div .component .compToggle>
            <div .toggleSelectorsContainer>
                $forall (sId, selectorWidget) <- selectors
                    <div .toggleSelector  data-toggle-id=#{sId}>
                        ^{selectorWidget}

            <div .toggleContentContainer>
                $forall (cId, comps) <- contents
                    <div .toggleContent data-toggle-id=#{cId}>
                        $forall c <- comps
                            ^{displayComponent c}
    |]