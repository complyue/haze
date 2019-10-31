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

import qualified RIO.Text                      as T
import           Text.Printf

import qualified RIO.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable  as VM
import qualified Data.Vector.Fusion.Bundle     as VG
import qualified Data.Vector.Generic.New       as VG

import qualified Data.Map                      as Map

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import           NeatInterpolation

default (Text, Int)


type ColumnDataSource = Map ColumnName ColumnData
type ColumnName = Text
-- | float64 is used uniformly for column data for now,
-- tho BokehJS can support more types, let's keep it simple.
-- note js within any browser inherently has no support of int64, 
-- float32/int32/int16/int8 worth to be added beyond just float64?
type ColumnData = VS.MVector (PrimState IO) Double


generateSeries :: Int -> (Int -> Double) -> UIO ColumnData
generateSeries n g = VG.runPrim $ VG.unstream $ VG.generate n g


type ArgName = Text
type CtorName = Text
type MethodName = Text
type AttrName = Text
type RangeName = Text
type AxisRef = Int

-- | define the value to be passed to BokehJS at js site
data BokehValue where
-- | will be converted to 'A.Value' before passed to js
    LiteralValue ::A.ToJSON a => a -> BokehValue
-- | reference a column from associated ColumnDataSource, in form of:
--   `{field: 'ccc'}`
    DataField ::ColumnName -> BokehValue
-- | some BokehJS methods work better with arg value in form of:
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
    plotGroup :: PlotGroup
    , plotWinId :: WindowId
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
    let pw = PlotWindow { plotGroup       = pg
                        , plotWinId       = winId
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
showPlot grpId plotProcedure =
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

        nla_ <- newIORef 0
        pws_ <- newIORef []
        let pg = PlotGroup { plotGrpId       = grpId
                           , numOfLinkedAxes = nla_
                           , windowsInGroup  = pws_
                           }

        plotProcedure pg

        !totalDataSize <- newIORef (0.0 :: Double)

        readIORef (windowsInGroup pg) >>= \pws -> for_ pws $ \pw -> do
            let
                sendCDS cds = do
                    for_ (Map.elems cds)
                        $ \cd -> modifyIORef'
                              totalDataSize
                              ((+) $ fromIntegral $ VM.length cd)
                            --  liftIO $ wsSendData wsc cd
            foldlDeque (const sendCDS) () (dsInWindow pw)

        -- TODO cont. here

        tds <- readIORef totalDataSize
        let msg =
                "plotting data size: "
                    <> (T.pack $ printf "%0.1f" (tds / 1024 / 1024))
                    <> " MB"
        liftIO $ wsSendText
            wsc
            [aesonQQ|{
"type": "msg"
, "msgText": #{msg}
}|]

        let plotCode = [text|
console.log('here it is!')
|]
        -- send json cmds and binary column data to UI
        liftIO $ wsSendText
            wsc
            [aesonQQ|{
"type": "call"
, "name": "plot"
, "args": [#{plotCode}]
}|]


