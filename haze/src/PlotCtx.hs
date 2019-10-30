{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module PlotCtx where

import           RIO


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
    plotGrpId :: Text
    , narrTimeline :: NarrTimeline
    , axes :: IORef (Map Text LinkedAxis)
}


data PlotWindow = PlotWindow {
    -- | `target` wrt openWindow in js, where `_blank` will
    -- always open new window
    webWindowRef :: Text
    -- | id of group to have all x-axis/y-axis sync'ed
    , plotGroup :: PlotGroup
}


data PlotFigure = PlotFigure {
    plotWindow :: PlotWindow
    , figureTitle :: IORef Text
    , figureTools :: IORef [Text]
    , figureSetup :: IORef [Text]
    , glyphSetup  :: IORef [Text]
    , legendSetup :: IORef [Text]
}


