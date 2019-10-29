{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Haze where

import           UIO

import           Data.Aeson.QQ                  ( aesonQQ )

default (Text, Int)


showHazePage :: Text -> UIO ()
showHazePage target = do
    -- log to front UI
    print "Showing haze page from Haskell code ..."

    -- log to backend
    logInfo $ display $ "Showing haze page in window " <> target

    -- do the trick
    uiComm [aesonQQ|{
"type": "call"
, "name": "openWindow"
, "args": ["haze.html", #{target}]
}|]

