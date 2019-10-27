{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Bokeh where

import           UIO

import           Data.Aeson.QQ                  ( aesonQQ )

default (Text, Int)


showBokehPage :: Text -> UIO ()
showBokehPage target = do
    -- log to front UI
    print "Showing bokeh page from Haskell code ..."

    -- log to backend
    logInfo $ display $ "Showing bokeh page in window " <> target

    -- do the trick
    uiComm [aesonQQ|{
"type": "call"
, "name": "openWindow"
, "args": ["bokeh.html", #{target}]
}|]

