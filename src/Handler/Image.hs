{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Image where

import Import
import qualified Data.Text as T (append)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)

imagesWidget :: Widget
imagesWidget = do
    allImages <- handlerToWidget $ runDB getAllImages
    let imageRows = fours allImages

    $(widgetFile "images-widget")

getAllImages :: DB [Entity Image]
getAllImages = selectList [] [Asc ImageCreated]

mkImagePath :: FilePath -> Image -> FilePath
mkImagePath dir img = dir
    ++ ('/' : (unpack $ imageUuid img))
    ++ ('.' : (toLower $ show $ imageExt img))

mkImageUrl :: Image -> Text
mkImageUrl img = "images/"
    `T.append` imageUuid img
    `T.append` "."
    `T.append` (pack $ toLower $ show $ imageExt img)

data ImageUpload = ImageUpload
    { iuFile :: FileInfo
    , iuName :: Text
    , iuTime :: UTCTime
    }

uploadForm :: Form ImageUpload
uploadForm = renderBootstrap4 BootstrapBasicForm $ ImageUpload
    <$> fileAFormReq "Choose an Image"
    <*> areq textField textSettings Nothing
    <*> lift (liftIO getCurrentTime)
    where textSettings = FieldSettings
            { fsLabel = "File name"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [
                --("class", "form-control")
                ]
            }

parseExt :: Text -> Maybe ImageExt
parseExt "image/jpeg" = Just JPG
parseExt "image/png" = Just PNG
parseExt "image/gif" = Just GIF
parseExt _ = Nothing