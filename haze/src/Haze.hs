{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Haze where

import           UIO

import qualified RIO.Vector                    as V

import qualified Data.Map                      as Map

import           NeatInterpolation

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import           PlotCtx

default (Text, Int)


openPlotWindow :: PlotGroup -> WindowId -> UIO PlotWindow
openPlotWindow pg winId = do
    dsiw <- newDeque
    pfs  <- newIORef []
    let pw = PlotWindow { plotWinId       = winId
                        , plotGroup       = pg
                        , dsInWindow      = dsiw
                        , figuresInWindow = pfs
                        }

    modifyIORef' (windowsInGroup pg) $ (:) pw

    return pw


putDataSource :: PlotWindow -> ColumnDataSource -> UIO DataSourceRef
putDataSource pw cds = do
    let !dsiw = dsInWindow pw
    dsr <- getDequeSize dsiw
    pushBackDeque dsiw cds
    return dsr


addPlotFigure :: PlotWindow -> (Map ArgName A.Value) -> UIO PlotFigure
addPlotFigure pw figArgs = do
    figureArgs' <- newIORef figArgs
    figureOps'  <- newIORef []
    linkedAxes' <- newIORef Map.empty
    let pf = PlotFigure { plotWindow = pw
                        , figureArgs = figureArgs'
                        , figureOps  = figureOps'
                        , linkedAxes = linkedAxes'
                        }

    modifyIORef' (figureArgs pf) $ flip Map.alter "tools" $ \case
        -- with tools if not explicitly specified, default to this list
        Nothing -> Just $ A.Array $ V.fromList
            [ "crosshair"
            , "pan"
            , "xwheel_zoom"
            , "ywheel_zoom"
            , "box_zoom"
            , "hover"
            , "undo"
            , "redo"
            , "reset"
            ]
        Just tools -> Just tools

    modifyIORef' (figuresInWindow pw) $ (:) pf

    return pf


onFigure :: PlotFigure -> FigureOp -> UIO ()
onFigure pf op = modifyIORef' (figureOps pf) $ (:) op


linkAxis :: PlotFigure -> RangeName -> AxisRef -> UIO ()
linkAxis pf rng axis = modifyIORef' (linkedAxes pf) $ Map.insert rng axis


defineAxis :: PlotGroup -> UIO AxisRef
defineAxis pg = atomicModifyIORef' (numOfLinkedAxes pg) $ \i -> (i + 1, i)


showPlot :: GroupId -> (PlotGroup -> UIO ()) -> UIO ()
showPlot grpId grpPlot = do
    nla <- newIORef 0
    pws <- newIORef []
    let pg = PlotGroup { plotGrpId       = grpId
                       , numOfLinkedAxes = nla
                       , windowsInGroup  = pws
                       }

    grpPlot pg

    -- TODO send json cmds out


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

