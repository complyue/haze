{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Haze.Types where

import           UIO

import qualified RIO.Vector.Storable           as VS

import qualified Data.Aeson                    as A


type ColumnDataSource = Map ColumnName ColumnData
type ColumnName = Text
-- | float64 is used uniformly for column data for now,
-- tho BokehJS can support more types, let's keep it simple.
-- note js within any browser inherently has no support of int64, 
-- float32/int32/int16/int8 worth to be added beyond just float64?
type ColumnData = VS.MVector (PrimState IO) Double
type DataSourceRef = Int


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


-- | operations can be performed against a figure
data FigureOp =
      AddGlyph MethodName DataSourceRef [(ArgName, BokehValue)]
    | AddLayout CtorName [(ArgName, BokehValue)]
    | SetFigAttrs [([AttrName], BokehValue)]
    | SetGlyphAttrs CtorName [([AttrName], BokehValue)]


