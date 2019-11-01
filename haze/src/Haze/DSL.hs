{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Haze.DSL
  ( ColumnDataSource
  , ColumnData
  , BokehValue(..)
  , PlotGroup(..)
  , PlotWindow(..)
  , PlotFigure(..)
  , uiPlot
  , openPlotWindow
  , putDataSource
  , addPlotFigure
  , ($@)
  , FigureOp(..)
  , linkAxis
  , linkAxes
  , defineAxis
  , AxisRef
  )
where

import           UIO

import qualified Data.Map                      as Map

import           Haze.Types
import           Haze.Plot


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
      ([ "crosshair"
       , "pan"
       , "xwheel_zoom"
       , "ywheel_zoom"
       , "box_zoom"
       , "hover"
       , "undo"
       , "redo"
       , "reset"
       ] :: [Text]
      )
    Just tools -> Just tools

  modifyIORef' (figuresInWindow pw) $ (:) pf

  return pf

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

