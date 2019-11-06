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

module Haze.DSL
    ( ColumnDataSource
    , ColumnData
    -- , BokehValue(..)
    -- , PlotGroup(..)
    -- , PlotWindow(..)
    -- , PlotFigure(..)
    -- , uiPlot
    -- , openPlotWindow
    -- , putDataSource
    -- , addPlotFigure
    -- , ($@)
    -- , FigureOp(..)
    -- , showPlot
    -- , linkAxis
    -- , linkAxes
    -- , defineAxis
    , AxisRef
    )
where

import           UIO
import qualified RIO.Text                      as T

import qualified Data.Map                      as Map

import qualified Data.Aeson                    as A

import           Haze.Types
import           Haze.Exec


-- | the pure monad to plot a group
newtype Plot a = Plot { doPlot :: PG -> (PG, a) }
instance Functor Plot where
    fmap f p = Plot $ \pg -> let (_, a) = doPlot p pg in (pg, f a)
instance Applicative Plot where
    pure x = Plot $ \pg -> (pg, x)
    (<*>) = ap
instance Monad Plot where
    m >>= f = Plot $ \pg -> let (pg', r) = doPlot m pg in doPlot (f r) pg'
instance MonadFail Plot where

-- | the pure monad to plot a window
newtype PlotWin a = PlotWin { doPlotWin :: PW -> (PW, a) }
instance Functor PlotWin where
    fmap f p = PlotWin $ \pw -> let (_, a) = doPlotWin p pw in (pw, f a)
instance Applicative PlotWin where
    pure x = PlotWin $ \pw -> (pw, x)
    (<*>) = ap
instance Monad PlotWin where
    m >>= f =
        PlotWin $ \pw -> let (pw', r) = doPlotWin m pw in doPlotWin (f r) pw'
instance MonadFail PlotWin where

-- | the pure monad to plot a figure
newtype PlotFig a = PlotFig { doPlotFig :: Fig -> (Fig, a) }
instance Functor PlotFig where
    fmap f p = PlotFig $ \pf -> let (_, a) = doPlotFig p pf in (pf, f a)
instance Applicative PlotFig where
    pure x = PlotFig $ \pf -> (pf, x)
    (<*>) = ap
instance Monad PlotFig where
    m >>= f =
        PlotFig $ \pf -> let (pf', r) = doPlotFig m pf in doPlotFig (f r) pf'
instance MonadFail PlotFig where


uiPlot :: GroupId -> Plot a -> UIO a
uiPlot pgId plotAct = do
    let (pg, x) = doPlot plotAct PG { pgId = pgId, lxs = 0, pws = [] }

    logInfo $ "plot has " <> (display $ length $ pws pg) <> " windows."

    return x


defineAxis :: Plot AxisRef
defineAxis = Plot $ \pg -> let !ar = lxs pg in (pg { lxs = ar + 1 }, ar)


type WinId = Text
type WinRef = Int
wPlot :: WinId -> PlotWin () -> Plot WinRef
wPlot pwId plotAct = Plot $ \pg ->
    let !wr       = length $ pws pg
        !pw       = PW { pwId = pwId, cdss = [], figs = [], lays = [] }
        !(pw', _) = doPlotWin plotAct pw
    in  (pg { pws = pw' : pws pg }, wr)


type FigRef = Int
fPlot :: BVC v => [(ArgName, v)] -> PlotFig () -> PlotWin FigRef
fPlot args plotAct = PlotWin $ \pw ->
    let !figNo_ = 1 + (length $ figs pw)
        !fig    = Fig { figArgs  = Map.map toBV (Map.fromList args)
                      , figOps   = []
                      , figLinks = Map.empty
                      }
        !(fig', _) = doPlotFig plotAct fig
    in  (pw { figs = fig' : figs pw }, figNo_)


setFigAttrs :: BVC v => [([AttrName], v)] -> PlotFig ()
setFigAttrs specs = PlotFig $ \pf ->
    let specs' = map (\(path, v) -> (path, toBV v)) specs
    in  (pf { figOps = SetFigAttrs' specs' : figOps pf }, ())


linkFigAxis :: RangeName -> AxisRef -> PlotFig ()
linkFigAxis rng axis =
    PlotFig $ \pf -> (pf { figLinks = Map.insert rng axis (figLinks pf) }, ())


