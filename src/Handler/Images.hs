{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Images where

import Import
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import System.Directory (removeFile, doesFileExist)
import Data.Time.Format.ISO8601
import Control.Arrow ((&&&))

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
        name = imageUuid img <> "."
                <> (pack $ toLower $ show $ imageExt img)

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

instance Eq Image where
    (==) i1 i2 = (==) (imageUuid i1) (imageUuid i2)

imageSelectField :: Field Handler (Entity Image)
imageSelectField = selectFieldHelper outerView noneView otherView opts
    where
        outerView = \idAttr nameAttr attrs inside -> do
            [whamlet|
                $newline never
                <input type="text" #imageSearch placeholder="Search for images...">
                <div ##{idAttr}>^{inside}
            |]
            toWidget
                [julius|
                    $(document).ready(function() {
                        $("#imageSearch").on("keyup", function() {
                            var value = $(this).val().toLowerCase();
                            $(".radioImageContainer p").filter(function() {
                                $(this).parent().parent().toggle($(this).text().toLowerCase().indexOf(value) > -1)
                            });
                        });
                    });
                |]
        noneView = \idAttr nameAttr isSel ->
            [whamlet|
                $newline never
                <label .radio for=#{idAttr}-none>
                    <div>
                        <input id=#{idAttr}-none type=radio name=#{nameAttr} value=none :isSel:checked>
                        _{MsgSelectNone}
            |]
        otherView = \idAttr nameAttr attrs value isSel text -> do
            opts' <- liftHandler opts
            let mimg = fmap entityVal $ (olReadExternal opts') value
            [whamlet|
                $newline never
                <label .radio for=#{idAttr}-#{value}>
                    <div>
                        <input .radioForImage id=#{idAttr}-#{value} type=radio name=#{nameAttr} value=#{value} :isSel:checked *{attrs}>
                        $maybe img <- mimg
                            <img .radioImage src=@{ImagesR $ mkImageUrl img}>
                        <p>#{text}
            |]
            toWidget
                [lucius|
                    .radioImage {
                        max-width: 120px;
                    }
                    
                    .radioForImage { 
                        position: absolute;
                        opacity: 0;
                        width: 0;
                        height: 0;
                    }
    
                    .radioImage {
                        cursor: pointer;
                    }

                    /* CHECKED STYLES */
                    .radioForImage:checked + img {
                        outline: 2px solid #f00;
                    }
                |]
        opts = do
            images <- runDB getAllImages
            let imageList = map (imageName . entityVal &&& id) images
            return $ mkOptions "image" imageList