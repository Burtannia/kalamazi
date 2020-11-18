{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Images where

import Import
import qualified Data.List as L (head)
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
                dir = appImageDir $ appSettings app
            
            case mExt of
                Nothing -> msgRedirect "Unsupported file type"
                Just ext -> do
                    let uuid = (iso8601Show $ iuTime uploadImg) <> "." <> (pack $ toLower $ show ext)
                        newImg = Image (pack uuid) (iuName uploadImg) ext (iuTime uploadImg)
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
        Nothing ->
            sendResponseStatus status404 ("Image not found" :: Text)
        Just img -> do
            minUseBy <- imageInUse imgId
            case minUseBy of
                Just name -> sendResponseStatus status403 $
                    "Image in use by " <> name
                Nothing -> do
                    app <- getYesod
                    let imgPath = mkImagePath (appImageDir $ appSettings app) img
                    liftIO $ removeFile imgPath
                    stillExists <- liftIO $ doesFileExist imgPath
                    unless stillExists $ do
                        runDB $ delete imgId
                        sendResponse ("Image Deleted" :: Text)   

imageInUse :: ImageId -> Handler (Maybe Text)
imageInUse imgId = do
    guides <- runDB $ selectList [] [Asc GuideModified]
    uses <- mapM (helper . entityVal) guides
    return $ listToMaybe $ catMaybes uses
    where
        helper g = do
            inUse <- guideUsesImg imgId g
            return $ if inUse
                then Just $ guideTitle g
                else Nothing

guideUsesImg :: ImageId -> Guide -> Handler Bool
guideUsesImg imgId g = do
    sections <- mapM (runDB . getJust) (guideSections g)
    let isIcon = guideIcon g == imgId
    return $ or $ isIcon : map (sectionUsesImg imgId) sections

sectionUsesImg :: ImageId -> Section -> Bool
sectionUsesImg imgId s = or $
    (Just imgId == sectionBackground s)
    : map (componentUsesImg imgId) (sectionContent s)

componentUsesImg :: ImageId -> Component -> Bool
componentUsesImg imgId (CToggle (ToggleImages bg ts)) =
    or $ map (imgId ==) (bg : map fst ts)
componentUsesImg _ _ = False

getAllImages :: DB [Entity Image]
getAllImages = selectList [] [Asc ImageCreated]

mkImagePath :: FilePath -> Image -> FilePath
mkImagePath dir img = dir ++ '/' : (unpack $ imageUuid img)

mkImageUrl :: ImageId -> Route Static
mkImageUrl imgId = StaticRoute [tshow imgId] []

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

imageSelectField :: Field Handler ImageId
imageSelectField = selectFieldHelper outerView noneView otherView opts
    where
        outerView = \idAttr nameAttr attrs inside -> do
            searchId <- newIdent
            [whamlet|
                $newline never
                <div>
                    <input type="text" ##{searchId} placeholder="Search for images...">
                    <div ##{idAttr}>^{inside}
            |]
            toWidget
                [julius|
                    $(document).ready(function() {
                        $("#" + #{searchId}).on("keyup", function() {
                            var value = $(this).val().toLowerCase();
                            $("#" + #{idAttr} + " .radioImageContainer p").filter(function() {
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
            let mimgId = (olReadExternal opts') value
            [whamlet|
                $newline never
                <label .radio for=#{idAttr}-#{value}>
                    <div .radioImageContainer>
                        <input .radioForImage id=#{idAttr}-#{value} type=radio name=#{nameAttr} value=#{value} :isSel:checked *{attrs}>
                        $maybe imgId <- mimgId
                            <img .radioImage src=@{ImagesR $ mkImageUrl imgId}>
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

                    .radioForImage:checked + img {
                        outline: 2px solid #f00;
                    }
                |]
        opts = do
            images <- runDB getAllImages
            let imageList = map (imageName . entityVal &&& entityKey) images
            return $ mkOptions "image" imageList