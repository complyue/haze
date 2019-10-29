{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module PlotCtx where

import           RIO

data PlotWindow = PlotWindow {
    -- | `target` wrt openWindow in js
    webTarget :: Text
    -- | id of group to have all x-axis/y-axis sync'ed
    , syncGroup :: Text
}


data PlotFigure = PlotFigure {
    figureTitle :: Text
    , figureSetup :: [Text]
}

