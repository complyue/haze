{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module Haze where

import           UIO

import qualified Data.Map                      as Map

import           NeatInterpolation

import           Data.Aeson.QQ                  ( aesonQQ )

import           PlotCtx

default (Text, Int)


withPlotGroup :: (PlotGroup -> UIO ()) -> UIO ()
withPlotGroup grpPlot = do
    axes' <- newIORef Map.empty
    let nla = LinkedAxis { axisName = "narr-timeline" }
        ntl = NarrTimeline { axis = nla }
        pg =
            PlotGroup { plotGrpId = "123" -- TODO use a global sequence or sth
                                         , narrTimeline = ntl, axes = axes' }

    grpPlot pg


withPlotWindow :: PlotGroup -> Text -> (PlotWindow -> UIO ()) -> UIO ()
withPlotWindow pg wRef winPlot = do
    let pw = PlotWindow { webWindowRef = wRef, plotGroup = pg }

    winPlot pw


withPlotFigure :: PlotWindow -> (PlotFigure -> UIO ()) -> UIO ()
withPlotFigure pw figPlot = do
    figureTitle' <- liftIO $ newIORef "Haze Plot"
    [figureTools', figureSetup', glyphSetup', legendSetup'] <- liftIO
        $ mapM newIORef [["zoom", "pan", "crosshair"], [], [], []]
    let pf = PlotFigure { plotWindow  = pw
                        , figureTitle = figureTitle'
                        , figureTools = figureTools'
                        , figureSetup = figureSetup'
                        , glyphSetup  = glyphSetup'
                        , legendSetup = legendSetup'
                        }

    figPlot pf

drawGlyph :: PlotFigure -> Text -> UIO ()
drawGlyph pf glType = do

    liftIO $ modifyIORef (glyphSetup pf) $ (:) [text|
${glType}()
        |text]


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

