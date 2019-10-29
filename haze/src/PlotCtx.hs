{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module PlotCtx where

import           RIO

import qualified Data.Map                      as Map


-- |
data LinkedAxis = LinkedAxis {
    axisName :: Text

}


-- |
data NarrTimeline = NarrTimeline {
    axis :: LinkedAxis
}


-- | a group has a single narrative timeline, and defines
-- a set of named axes to be linked in pan/zoom
data PlotGroup = PlotGroup {
    narrTimeline :: NarrTimeline
    , axes :: Map Text LinkedAxis
}


data PlotWindow = PlotWindow {
    -- | `target` wrt openWindow in js
    webTarget :: Text
    -- | id of group to have all x-axis/y-axis sync'ed
    , syncGroup :: Text
}


data PlotFigure = PlotFigure {
    figureTitle :: Text
    , figureTools :: [Text]
    , figureSetup :: [Text]
}


