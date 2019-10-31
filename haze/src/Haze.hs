{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Haze where

import           UIO
import           HaduiUtil

import qualified RIO.Vector.Storable           as VS

import qualified Data.Map                      as Map

import           NeatInterpolation

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

default (Text, Int)


type ColumnDataSource = Map ColumnName ColumnData
type ColumnName = Text
type ColumnData = VS.MVector (PrimState IO) Double

type ArgName = Text
type CtorName = Text
type MethodName = Text
type AttrName = Text
type RangeName = Text
type AxisRef = Int

data BokehValue where
    LiteralValue ::A.ToJSON a => a -> BokehValue
-- | reference a column from associated ColumnDataSource, in form of:
--   `{field: 'nnn'}`
    DataField ::ColumnName -> BokehValue
-- | some bokehjs methods works better with arg value in form of:
--   `{value: vvv}`
    DataValue ::A.ToJSON a => a -> BokehValue
-- | call a constructor at js site to create the value, in form of:
--   `new Bokeh.Ccc({kkk: vvv, ...})`
    NewBokehObj ::CtorName -> [(ArgName, BokehValue)] -> BokehValue


-- | a group has a single narrative timeline, and defines
-- a set of named axes to be linked in pan/zoom
data PlotGroup = PlotGroup {
    plotGrpId :: GroupId
    , numOfLinkedAxes :: IORef Int
    , windowsInGroup :: IORef [PlotWindow]
}
type GroupId = Text


-- | a window mapped to a browser window (tab actually nowadays)
data PlotWindow =  PlotWindow {
    plotWinId :: WindowId
    , plotGroup :: PlotGroup
    , dsInWindow :: BDeque (PrimState IO) ColumnDataSource
    , figuresInWindow :: IORef [PlotFigure]
}
type WindowId = Text


-- | a figure with glyphs and layout elements
data PlotFigure = PlotFigure {
    plotWindow :: PlotWindow
    , figureArgs :: IORef (Map ArgName BokehValue)
    , figureOps  :: IORef [FigureOp]
    , linkedAxes :: IORef (Map RangeName AxisRef)
}


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


putDataSource :: PlotWindow -> [(ColumnName, ColumnData)] -> UIO DataSourceRef
putDataSource pw cds = do
    let !dsiw = dsInWindow pw
    dsr <- getDequeSize dsiw
    pushBackDeque dsiw $ Map.fromList cds
    return dsr

-- todo other types of array element to support ?
-- note js within any browser inherently has no support of int64, 
-- int32/int16/int8 worth to be added beyond float64 ?
type DataSourceRef = Int


addPlotFigure :: PlotWindow -> [(ArgName, BokehValue)] -> UIO PlotFigure
addPlotFigure pw figArgs = do
    figureArgs' <- newIORef $ Map.fromList figArgs
    figureOps'  <- newIORef []
    linkedAxes' <- newIORef Map.empty
    let pf = PlotFigure { plotWindow = pw
                        , figureArgs = figureArgs'
                        , figureOps  = figureOps'
                        , linkedAxes = linkedAxes'
                        }

    modifyIORef' (figureArgs pf) $ flip Map.alter "tools" $ \case
        -- the list of tools if not explicitly specified, defaults to this list
        Nothing -> Just $ LiteralValue
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


data FigureOp =   SetFigAttrs [([AttrName], BokehValue)]
    | AddGlyph MethodName DataSourceRef [(ArgName, BokehValue)]
    | AddLayout CtorName [(ArgName, BokehValue)]
    | SetGlyphAttrs CtorName [([AttrName], BokehValue)]

infixr 0 $@ -- be infixr so can work together with ($)
-- | perform op on a figure
($@) :: PlotFigure -> FigureOp -> UIO ()
pf $@ op = modifyIORef' (figureOps pf) $ (:) op


linkAxis :: AxisRef -> RangeName -> PlotFigure -> UIO ()
linkAxis axis rng pf = modifyIORef' (linkedAxes pf) $ Map.insert rng axis

linkAxes :: PlotGroup -> RangeName -> [PlotFigure] -> UIO AxisRef
linkAxes pg rng pfs = do
    axis <- defineAxis pg
    -- TODO check all pfs belong to pg
    for_ pfs $ linkAxis axis rng
    return axis

defineAxis :: PlotGroup -> UIO AxisRef
defineAxis pg = atomicModifyIORef' (numOfLinkedAxes pg) $ \i -> (i + 1, i)


showPlot :: GroupId -> (PlotGroup -> UIO ()) -> UIO ()
showPlot grpId grpPlot =
    (ask >>= \uio ->
        let gil = haduiGIL uio
        in
            isEmptyMVar gil >>= \case
                True ->
                    logError
                        $  display
                        $  "No ws in context to plot group "
                        <> grpId
                False -> readMVar gil >>= plotViaWS
    )
  where
    plotViaWS wsc = do

        nla <- newIORef 0
        pws <- newIORef []
        let pg = PlotGroup { plotGrpId       = grpId
                           , numOfLinkedAxes = nla
                           , windowsInGroup  = pws
                           }

        grpPlot pg

        -- send json cmds and binary column data to UI
        liftIO $ wsSendText
            wsc
            [aesonQQ|{
"type": "call"
, "name": "xxx"
, "args": []
}|]


