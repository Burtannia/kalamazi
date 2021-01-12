{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Summernote where

import Import
import           Text.HTML.SanitizeXSS           (sanitizeBalance)
import           Text.Julius                     (rawJS)

snFieldUnsanitized :: Field Handler Html
snFieldUnsanitized = snField' sanitizeBalance

snField :: Field Handler Html
snField = snField' id

snField' :: (Text -> Text) -> Field Handler Html
snField' f = Field
    { fieldParse = parseHelper (Right . preEscapedToMarkup . f)
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq -> do
        addStylesheet $ StaticR summernote_summernotebs4_css
        addScript $ StaticR summernote_summernotebs4_js
        [whamlet|
            <textarea id=#{idAttr} name=#{nameAttr} *{otherAttrs} onchange="remakeSn(this)">#{showRes eResult}
        |]
        toWidget
            [julius|
                $(document).ready(function() {
                    $('##{rawJS idAttr}').summernote(summernoteSettings);
                });

                function remakeSn(e) {
                    $(e).summernote('destroy');
                    $(e).summernote(summernoteSettings);
                    $(e).next('.note-editor').next('.note-editor').remove(); //horrible hack to remove duplicate
                }
            |]
    , fieldEnctype = UrlEncoded
    }
    where
        showRes = either toHtml id