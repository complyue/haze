{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

-- | The embedded DSL provided by Haze to facilitate plotting specification
-- in pure monads. 

module Haze.DSL
    (
-- * Pure monads for plotting
      Plot
    , PlotWin
    , PlotFig
-- * Entry point to plotting works
    , uiPlot
-- * Actions in a plot group
    , defineAxis
    , openWindow
-- * Actions in a plot window
    , putDataSource
    , figure
    , showPlot
-- * Actions in a plot figure
    , setFigAttrs
    , addGlyph
    , addLayout
    , setGlyphAttrs
    , linkFigAxis
-- * Types to name arguments & return values of API functions
    , PlotGrpId
    , PlotWinId
    , ArgName
    , CtorName
    , MethodName
    , AttrName
    , RangeName
    , AxisRef
    , WinRef
    , DataSourceRef
    , FigRef
    , ColumnDataSource
    , ColumnName
    , ColumnData
    , BokehExpr(..)
    , dataField
    , dataValue
    , newBokehObj
    , BokehValue(..)
-- * Helper stuff
    , jsNull
    )
where

import           UIO

import qualified Data.Map                      as Map

import           Haze.Types
import           Haze.Monad
import           Haze.Exec


-- | multiple ranges from multiple windows/figures can be synchronized,
-- so they zoom/pan together, the link target in 'linkFigAxis' is defined
-- by this function
defineAxis :: Plot AxisRef
defineAxis = Plot $ \pg ->
    let !ar = numSyncAxes pg in (pg { numSyncAxes = ar + 1 }, AxisRef ar)


-- | open a new plot window
openWindow
    :: PlotWinId -- ^ a browser window identified by 'PlotGrpId' + 'PlotWinId'
                 -- is reused across multiple plot works
    -> PlotWin () -- ^ specify figures for this window with this monadic
                  -- computation
    -> Plot WinRef
openWindow winId plotAct = Plot $ \pg ->
    let
        !wr = length $ pltWins pg
        !pw = PltWin { pwId        = winId
                     , colDataSrcs = []
                     , pltFigs     = []
                     , pltLays     = []
                     }
        !(pw', _) = doPlotWin plotAct pw
    in
        (pg { pltWins = pw' : pltWins pg }, WinRef wr)

-- | put a data source into current plot window
putDataSource :: [(ColumnName, ColumnData)] -> PlotWin DataSourceRef
putDataSource cdl = PlotWin $ \pw ->
    let !cdss = colDataSrcs pw
        !dsr  = DataSourceRef $ length cdss
    in  (pw { colDataSrcs = Map.fromList cdl : cdss }, dsr)

-- | Create a new Figure for plotting.
-- see:
--   [Bokeh API reference](https://docs.bokeh.org/en/latest/docs/reference/plotting.html#bokeh.plotting.figure.figure)
figure :: BokehValue v => [(ArgName, v)] -> PlotFig () -> PlotWin FigRef
figure args plotAct = PlotWin $ \pw ->
    let !figNo = length $ pltFigs pw
        !fig   = PltFig { figArgs  = Map.map bokehExpr (Map.fromList args)
                        , figOps   = []
                        , figLinks = Map.empty
                        }
        !(fig', _) = doPlotFig plotAct fig
    in  (pw { pltFigs = fig' : pltFigs pw }, FigRef figNo)


-- | it needs your familarity with [Bokeh API](https://docs.bokeh.org) and some
-- imagination to get the paths and values right here :-/
setFigAttrs
    :: BokehValue v
    => [([AttrName], v)] -- ^ a list of (path, value) pairs to set to current figure
    -> PlotFig ()
setFigAttrs specs = PlotFig $ \pf ->
    let specs' = map (\(path, v) -> (path, bokehExpr v)) specs
    in  (pf { figOps = SetFigAttrs specs' : figOps pf }, ())

-- | Figure objects have many glyph methods that can be used to draw vectorized
-- graphical glyphs, see:
--   [Bokeh API reference](https://docs.bokeh.org/en/latest/docs/reference/plotting.html#bokeh.plotting.figure.Figure)
addGlyph
    :: BokehValue v
    => MethodName
    -> DataSourceRef
    -> [(ArgName, v)]
    -> PlotFig ()
addGlyph mth dsr args = PlotFig $ \pf ->
    let !fops = figOps pf
        !op   = AddGlyph mth dsr $ Map.map bokehExpr $ Map.fromList args
    in  (pf { figOps = op : fops }, ())

-- | Adds an object to the plot in a specified place, see:
--   [Bokeh Guide](https://docs.bokeh.org/en/latest/docs/user_guide/annotations.html?#adding-annotations)
--   [Bokeh API reference](https://docs.bokeh.org/en/latest/docs/reference/models/plots.html?#bokeh.models.plots.Plot.add_layout)
addLayout
    :: BokehValue v
    => CtorName -- ^ the 'CtorName' must reside within the `Bokeh` namespace
    -> [(ArgName, v)]
    -> PlotFig ()
addLayout ctor args = PlotFig $ \pf ->
    let !fops = figOps pf
        !op   = AddLayout ctor $ Map.map bokehExpr $ Map.fromList args
    in  (pf { figOps = op : fops }, ())

-- | it needs your familarity with [Bokeh API](https://docs.bokeh.org) and some
-- imagination to get the paths and values right here :-/
setGlyphAttrs
    :: BokehValue v
    => CtorName -- ^ the 'CtorName' must reside within the `Bokeh` namespace
    -> [([AttrName], v)] -- ^ a list of (path, value) pairs to set to current figure
    -> PlotFig ()
setGlyphAttrs ctor specs = PlotFig $ \pf ->
    let specs' = map (\(path, v) -> (path, bokehExpr v)) specs
    in  (pf { figOps = SetGlyphAttrs ctor specs' : figOps pf }, ())


-- | link the specified range to a pre-defined sync axis in the plot group
linkFigAxis :: RangeName -> AxisRef -> PlotFig ()
linkFigAxis rng axis =
    PlotFig $ \pf -> (pf { figLinks = Map.insert rng axis (figLinks pf) }, ())


-- | need some lucky guess to use this correctly, after grokking the example, see:
--   [BokehJS example](https://docs.bokeh.org/en/latest/docs/user_guide/bokehjs.html#minimal-example)
--   [Bokeh API reference](https://docs.bokeh.org/en/latest/docs/reference/layouts.html#bokeh-layouts)
--
-- bonus: checkout <https://github.com/bokeh/bokeh/blob/master/bokehjs/src/lib/api/plotting.ts#L959>
showPlot
    :: (BokehValue c, BokehValue v)
    => MethodName -- ^ the 'MethodName' must reside within the `Bokeh.Plotting` namespace
    -> c
    -> [(ArgName, v)]
    -> BokehExpr
    -> PlotWin ()
showPlot mth children args target = PlotWin $ \pw ->
    let !lays = pltLays pw
        !lay  = PltLay mth
                       (bokehExpr children)
                       (Map.map bokehExpr $ Map.fromList args)
                       target
    in  (pw { pltLays = lay : lays }, ())

