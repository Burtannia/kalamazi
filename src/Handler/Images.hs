{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Images where

import Import
import qualified Data.Text as T (append)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import System.Directory (removeFile, doesFileExist)
import Data.Time.Format.ISO8601

-- maximumContentLength _ (Just ImagesR) = Just $ 200 * 1024 * 1024 -- 200 megabytes
-- maximumContentLength _ _ = Just $ 10 * 1024 * 1024 -- 10 megabytes
-- limit files to images
imagesWidget :: Widget
imagesWidget = do
    allImages <- handlerToWidget $ runDB getAllImages
    let imageRows = fours allImages

    $(widgetFile "images-widget")

getImageManagerR :: Handler Html
getImageManagerR = do
    (formWidget, enctype) <- generateFormPost uploadForm
    defaultLayout $ do
        setTitle "Image Manager"
        $(widgetFile "image-manager")

postImageManagerR :: Handler Html
postImageManagerR = do
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
        setTitle "Image Manager"
        $(widgetFile "image-manager")

msgRedirect :: Html -> Handler ()
msgRedirect msg = do
    setMessage msg
    redirect ImageManagerR

deleteImageR :: ImageId -> Handler ()
deleteImageR imgId = do
    mImg <- runDB $ get imgId
    
    case mImg of
        Nothing -> return ()
            -- add proper error messages
        Just img -> do
            app <- getYesod
            let imgPath = mkImagePath (appImageDir $ appSettings app) img
            liftIO $ removeFile imgPath
            stillExists <- liftIO $ doesFileExist imgPath

            unless stillExists $ do
                runDB $ delete imgId
                return () -- add proper message        

getAllImages :: DB [Entity Image]
getAllImages = selectList [] [Asc ImageCreated]

mkImagePath :: FilePath -> Image -> FilePath
mkImagePath dir img = dir
    ++ ('/' : (unpack $ imageUuid img))
    ++ ('.' : (toLower $ show $ imageExt img))

mkImageUrl :: Image -> Route Static
mkImageUrl img = StaticRoute [name] []
    where
        name = imageUuid img
                `T.append` "."
                `T.append` (pack $ toLower $ show $ imageExt img)

data ImageUpload = ImageUpload
    { iuFile :: FileInfo
    , iuName :: Text
    , iuTime :: UTCTime
    }

uploadForm :: Form ImageUpload
uploadForm = renderBootstrap4 BootstrapBasicForm $ ImageUpload
    <$> fileAFormReq fileSettings
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
          fileSettings = FieldSettings
            { fsLabel = "File name"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [
                    ("accept", ".jpg, .png, .gif")
                ]
            }


parseExt :: Text -> Maybe ImageExt
parseExt "image/jpeg" = Just JPG
parseExt "image/png" = Just PNG
parseExt "image/gif" = Just GIF
parseExt _ = Nothing