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

import qualified RIO.Vector                    as V
import qualified RIO.Vector.Storable           as VS

import qualified Data.Map                      as Map

import           NeatInterpolation

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Network.WebSockets            as WS


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


addPlotFigure :: PlotWindow -> [(ArgName, A.Value)] -> UIO PlotFigure
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


type AxisRef = Int


type GroupId = Text
-- | a group has a single narrative timeline, and defines
-- a set of named axes to be linked in pan/zoom
data PlotGroup = PlotGroup {
    plotGrpId :: GroupId
    , numOfLinkedAxes :: IORef Int
    , windowsInGroup :: IORef [PlotWindow]
}


type WindowId = Text
-- | a window mapped to a browser window (tab actually nowadays)
data PlotWindow =  PlotWindow {
    plotWinId :: WindowId
    , plotGroup :: PlotGroup
    , dsInWindow :: BDeque (PrimState IO) ColumnDataSource
    , figuresInWindow :: IORef [PlotFigure]
}


type ColumnName = Text
type ColumnData = VS.MVector (PrimState IO) Double
-- todo other types of array element to support ?
-- note js within any browser inherently has no support of int64, 
-- int32/int16/int8 worth to be added beyond float64 ?
type ColumnDataSource = Map ColumnName ColumnData

type DataSourceRef = Int


type RangeName = Text
-- | a figure with glyphs and layout elements
data PlotFigure = PlotFigure {
    plotWindow :: PlotWindow
    , figureArgs :: IORef (Map ArgName A.Value)
    , figureOps  :: IORef [FigureOp]
    , linkedAxes :: IORef (Map RangeName AxisRef)
}


type CtorName = Text
type MethodName = Text
type ArgName = Text
type AttrName = Text

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
    NewBokehObj ::CtorName -> (Map ArgName (BokehValue)) -> BokehValue

newBokehObj :: CtorName -> [(ArgName, BokehValue)] -> BokehValue
newBokehObj ctor args = NewBokehObj ctor $ Map.fromList args

data FigureOp =   SetFigAttrs [([AttrName], BokehValue)]
    | AddGlyph MethodName DataSourceRef (Map ArgName (BokehValue))
    | AddLayout CtorName (Map ArgName (BokehValue))
    | SetGlyphAttrs CtorName [([AttrName], BokehValue)]

addGlyph :: MethodName -> DataSourceRef -> [(ArgName, BokehValue)] -> FigureOp
addGlyph mth ds args = AddGlyph mth ds $ Map.fromList args

addLayout :: CtorName -> [(ArgName, BokehValue)] -> FigureOp
addLayout ctor args = AddLayout ctor $ Map.fromList args

infixr 0 $@ -- be infixr so can work together with ($)
-- | perform op on a figure
($@) :: PlotFigure -> FigureOp -> UIO ()
pf $@ op = modifyIORef' (figureOps pf) $ (:) op


linkAxis :: PlotFigure -> RangeName -> AxisRef -> UIO ()
linkAxis pf rng axis = modifyIORef' (linkedAxes pf) $ Map.insert rng axis

linkAxes :: PlotGroup -> [(PlotFigure, RangeName)] -> UIO AxisRef
linkAxes pg prs = do
    axis <- defineAxis pg
    for_ prs $ \(pf, rng) -> linkAxis pf rng axis
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


