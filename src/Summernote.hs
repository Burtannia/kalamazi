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
                    $('##{rawJS idAttr}').summernote({
                        height: 200,
                        minHeight: 150,
                        colors: [
                            // Rarity Colours
                            ['#9d9d9d', '#ffffff', '#1eff00', '#0070dd', '#a335ee', '#ff8000', '#e6cc80', '#00ccff'],
                            // Class Colours
                            ['#C41E3A', '#A330C9', '#FF7C0A', '#AAD372', '#3FC7EB', '#00FF98', '#F48CBA', '#FFFFFF'],
                            ['#FFF468', '#0070DD', '#8788EE', '#C69B6D']
                        ],
                        colorsName: [
                            ["Poor", "Common", "Uncommon", "Rare", "Epic", "Legendary", "Artifact", "Heirloom"],
                            ["Death Knight", "Demon Hunter", "Druid", "Hunter", "Mage", "Monk", "Paladin", "Priest"],
                            ["Rogue", "Shaman", "Warlock", "Warrior"]
                        ],
                        styleTags: [
                            { title: 'Header', tag: 'h3', className: 'comp-header', value: 'h3' },
                            { title: 'Sub-Header', tag: 'h5', className: 'comp-sub-header', value: 'h5' },
                            { title: 'Content', tag: 'p', className: 'comp-content', value: 'p' },
                            { title: 'Footnote', tag: 'p', className: 'comp-footnote', value: 'p' }
                        ],
                        toolbar: [
                            ['style', ['style', 'bold', 'italic', 'underline', 'clear']],
                            ['fontsize', ['fontsize']],
                            ['color', ['color']],
                            ['para', ['ul', 'ol', 'paragraph']]
                        ]
                    });
                });

                function remakeSn(e) {
                    $(e).summernote('destroy');
                    $(e).summernote();
                    $(e).next('.note-editor').next('.note-editor').remove(); //horrible hack to remove duplicate
                }
            |]
    , fieldEnctype = UrlEncoded
    }
    where
        showRes = either toHtml id