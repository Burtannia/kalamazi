{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Poke where

import Import
import Handler.Section (updateGuideModified)

patchPokeR :: Handler ()
patchPokeR = do
    guideIds <- fmap (map entityKey) $ runDB $ selectList [] [Asc GuideTitle]
    mapM_ updateGuideModified guideIds
    sendResponse ("Guides poked" :: Text)