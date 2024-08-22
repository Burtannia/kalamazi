{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Images where

import Data.Time.Format.ISO8601
import Import
import System.Directory (doesFileExist, removeFile)
import Yesod.Core.Types (HandlerContents (..))
import Yesod.Form.Bootstrap4

getImageManager :: Widget
getImageManager = do
    (formWidget, enctype) <-
        liftHandler $
            genBs4FormIdentify' BootstrapInlineForm imageFormId uploadForm

    (massFormWidget, massEnctype) <-
        liftHandler $
            genBs4FormIdentify' BootstrapInlineForm massImageFormId massImageForm

    modalId <- newIdent
    $(widgetFile "image-manager")

postImageManager :: Widget
postImageManager = do
    ((result, formWidget), enctype) <-
        liftHandler $
            runBs4FormIdentify' BootstrapInlineForm imageFormId uploadForm

    ((massFormResult, massFormWidget), massEnctype) <-
        liftHandler $
            runBs4FormIdentify' BootstrapInlineForm massImageFormId massImageForm

    modalId <- newIdent

    case result of
        FormSuccess iu -> liftHandler $ do
            _ <- uploadImage (Just $ iuName iu) (iuFile iu)
            msgRedirect "Image uploaded successfully"
        FormMissing -> return ()
        FormFailure errs -> do
            liftIO $ putStrLn "postImageManager: Image Upload"
            print errs

    case massFormResult of
        FormSuccess _ ->
            liftHandler $
                msgRedirect "Images uploaded successfully"
        FormMissing -> return ()
        FormFailure errs -> do
            liftIO $ putStrLn "postImageManager: Mass Image Upload"
            print errs

    $(widgetFile "image-manager")

imageFormId :: Text
imageFormId = "upload-image"

massImageFormId :: Text
massImageFormId = "mass-upload-image"

imagesWidget :: Widget
imagesWidget = do
    imgs <- handlerToWidget $ runDB getAllImages
    $(widgetFile "images-widget")

deleteImageR :: ImageId -> Handler ()
deleteImageR imgId = do
    mImg <- runDB $ get imgId
    case mImg of
        Nothing ->
            sendResponseStatus status404 ("Image not found" :: Text)
        Just img -> do
            minUseBy <- imageInUse imgId
            case minUseBy of
                Just name ->
                    sendResponseStatus status403 $
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
        return $
            if inUse
                then Just $ guideTitle g
                else Nothing

guideUsesImg :: ImageId -> Guide -> Handler Bool
guideUsesImg imgId g = do
    sections <- mapM (runDB . getJust) (guideSections g)
    let isIcon = guideIcon g == imgId
    return $ or $ isIcon : map (sectionUsesImg imgId) sections

sectionUsesImg :: ImageId -> Section -> Bool
sectionUsesImg imgId s =
    or $
        map (componentUsesImg imgId) (sectionContent s)

componentUsesImg :: ImageId -> Component -> Bool
componentUsesImg imgId (CToggle (ToggleImages ts)) =
    or $ map (\x -> imgId == fst x) ts
componentUsesImg imgId (CTalents (TalentConfig{talentPreview})) =
    talentPreview == imgId
componentUsesImg imgId (CHeroTalents (HeroTalentConfig{talentPreview})) =
    talentPreview == imgId
componentUsesImg _ _ = False

getAllImages :: DB [Entity Image]
getAllImages = selectList [] [Asc ImageCreated]

mkImagePath :: FilePath -> Image -> FilePath
mkImagePath dir img = dir ++ '/' : (unpack $ imageUuid img)

mkImageUrl :: ImageId -> Route Static
mkImageUrl imgId = StaticRoute [toPathPiece imgId] []

data ImageUpload = ImageUpload
    { iuFile :: FileInfo
    , iuName :: Text
    }

uploadImage :: Maybe Text -> FileInfo -> Handler ImageId
uploadImage mName file = do
    app <- getYesod

    now <- liftIO getCurrentTime

    let mExt = parseExt $ fileContentType file
        dir = appImageDir $ appSettings app

    case mExt of
        Nothing -> liftIO $ throwIO $ HCError $ InvalidArgs ["Unsupported file type"]
        Just ext -> do
            let uuid = (iso8601Show now) <> "." <> (pack $ toLower $ show ext)
                imgName = fromMaybe (dropExt $ fileName file) mName
                newImg = Image (pack uuid) imgName ext now
            liftIO $ fileMove file $ mkImagePath dir newImg
            imgId <- liftHandler $ runDB $ insert newImg
            return imgId

uploadForm :: AForm Handler ImageUpload
uploadForm =
    ImageUpload
        <$> fileAFormReq fileSettings
        <*> areq textField nameSettings Nothing
  where
    fileSettings =
        FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("accept", ".jpg, .png, .gif")
                , ("class", "form-control-file mr-sm-2 mb-ltsmall")
                ]
            }
    nameSettings =
        FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("placeholder", "Image Name")
                , ("class", "form-control mr-sm-2 mb-ltsmall")
                ]
            }

parseExt :: Text -> Maybe ImageExt
parseExt "image/jpeg" = Just JPG
parseExt "image/png" = Just PNG
parseExt "image/gif" = Just GIF
parseExt _ = Nothing

imageSelectField :: [Entity Image] -> Field Handler ImageId
imageSelectField = imageSelectFieldHelper False

imageSelectFieldToggle :: [Entity Image] -> Field Handler ImageId
imageSelectFieldToggle = imageSelectFieldHelper True

imageSelectFieldHelper :: Bool -> [Entity Image] -> Field Handler ImageId
imageSelectFieldHelper toggle images = selectFieldHelper outerView noneView otherView (return opts)
  where
    outerView = \idAttr _ _ inside -> do
        [whamlet|
                $newline never
                $if toggle
                    <button .btn .btn-dark .btn-block .ml-0 .mb-3
                        type="button" data-toggle="collapse" onclick="toggleNext(this)">Select Image

                <div :toggle:.collapse>
                    <input .form-control type="text" ##{idAttr <> "-search"} placeholder="Search for images..."
                        onkeyup="searchImages(this)" onchange="showImages(this)">
                    <div ##{idAttr} .row .image-list .max-h-60 .mt-3 style="padding-top: 3px">^{inside}
            |]
    noneView = \idAttr nameAttr isSel ->
        [whamlet|
                $newline never
                <div .col-3>
                    <label .radio .mb-0 for=#{idAttr}-none>
                        <div>
                            <input id=#{idAttr}-none type=radio name=#{nameAttr} value=none :isSel:checked>
                            _{MsgSelectNone}
            |]
    otherView = \idAttr nameAttr attrs value isSel text -> do
        let mimgId = (olReadExternal opts) value
        [whamlet|
                $newline never
                <div .col-3>
                    <label .radio .mb-0 for=#{idAttr}-#{value}>
                        <div .radioImageContainer>
                            <input .radioForImage id=#{idAttr}-#{value} type=radio name=#{nameAttr} value=#{value} :isSel:checked *{attrs}>
                            $maybe imgId <- mimgId
                                <img loading="lazy" .img-fluid src=@{ImagesR $ mkImageUrl imgId}>
                            <p .text-center>#{text}
            |]
    opts = mkImageOpts images

mkImageOpts :: [Entity Image] -> OptionList ImageId
mkImageOpts images = mkOptions "image" imageList
  where
    imageList = map (imageName . entityVal &&& entityKey) images

massImageForm :: AForm Handler [ImageId]
massImageForm = areq multiImageField fs Nothing
  where
    fs =
        FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("accept", ".jpg, .png, .gif")
                , ("class", "form-control-file mr-sm-2 mb-ltsmall")
                ]
            }

multiImageField :: Field Handler [ImageId]
multiImageField =
    Field
        { fieldParse = \_ files ->
            if null files
                then return $ Right Nothing
                else do
                    imgs <- mapM (uploadImage Nothing) files
                    return $ Right $ Just imgs
        , fieldView = \id' name attrs _ isReq ->
            [whamlet|
            <input ##{id'} name=#{name} *{attrs} type=file :isReq:required multiple>
        |]
        , fieldEnctype = Multipart
        }