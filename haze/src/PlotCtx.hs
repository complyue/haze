{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}

module PlotCtx where

import           RIO
import qualified RIO.Vector.Storable           as VS

import qualified Data.Aeson                    as A


type AxisRef = Int


type GroupId = Text
-- | a group has a single narrative timeline, and defines
-- a set of named axes to be linked in pan/zoom
data PlotGroup = PlotGroup {
    plotGrpId :: GroupId
    , numOfLinkedAxes :: IORef Int
    , windowsInGroup :: IORef [PlotWindow]
}


type ColumnName = Text
type ColumnData = VS.MVector (PrimState IO) Double
-- todo other types of array element to support ?
-- note js within any browser inherently has no support of int64, 
-- int32/int16/int8 worth to be added beyond float64 ?
type ColumnDataSource = Map ColumnName ColumnData


type WindowId = Text
-- | a window mapped to a browser window (tab actually nowadays)
data PlotWindow =  PlotWindow {
    plotWinId :: WindowId
    , plotGroup :: PlotGroup
    , dsInWindow :: BDeque (PrimState IO) ColumnDataSource
    , figuresInWindow :: IORef [PlotFigure]
}


type DataSourceRef = Int
type ArgName = Text
data ArgValue = DataField ColumnName | DataValue A.Value  | LiteralValue A.Value

type MethodName = Text
type AttrName = Text
data FigureOp = FigureCall MethodName (Maybe DataSourceRef) (Map ArgName ArgValue)
    | FigureUpdate [AttrName] A.Value
    | FigureMod MethodName A.Value


type AxisField = Text
-- | a figure with glyphs and layout elements
data PlotFigure = PlotFigure {
    plotWindow :: PlotWindow
    , figureArgs :: IORef (Map ArgName A.Value)
    , figureOps  :: IORef [FigureOp]
    , linkedAxes :: IORef (Map AxisField AxisRef)
}

