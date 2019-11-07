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

-- | The pure monads to be used in Haze DSL to define plotting specifications
--
-- You normally just import 'Haze' to use the DSL, and only need to import
-- this module for some reason you need the field accessors of the monads.

module Haze.Monad where

import           UIO

import           Control.Monad

import           Haze.Types


-- | the pure monad to plot a group
newtype Plot a = Plot { doPlot :: PltGrp -> (PltGrp, a) }
instance Functor Plot where
    fmap f p = Plot $ \pg -> let (pg', a) = doPlot p pg in (pg', f a)
instance Applicative Plot where
    pure x = Plot $ \pg -> (pg, x)
    (<*>) = ap
instance Monad Plot where
    m >>= f =
        Plot $ \pg -> let !(pg', r) = doPlot m pg in doPlot (f $! r) $! pg'
    m >> m' = Plot $ \pg -> let !(pg', _) = doPlot m pg in doPlot m' $! pg'

-- | the pure monad to plot a window
newtype PlotWin a = PlotWin { doPlotWin :: PltWin -> (PltWin, a) }
instance Functor PlotWin where
    fmap f p = PlotWin $ \pw -> let (pw', a) = doPlotWin p pw in (pw', f a)
instance Applicative PlotWin where
    pure x = PlotWin $ \pw -> (pw, x)
    (<*>) = ap
instance Monad PlotWin where
    m >>= f =
        PlotWin $ \pw ->
            let !(pw', r) = doPlotWin m pw in doPlotWin (f $! r) $! pw'
    m >> m' = PlotWin
        $ \pw -> let !(pw', _) = doPlotWin m pw in doPlotWin m' $! pw'

-- | the pure monad to plot a figure
newtype PlotFig a = PlotFig { doPlotFig :: PltFig -> (PltFig, a) }
instance Functor PlotFig where
    fmap f p = PlotFig $ \pf -> let (pf', a) = doPlotFig p pf in (pf', f a)
instance Applicative PlotFig where
    pure x = PlotFig $ \pf -> (pf, x)
    (<*>) = ap
instance Monad PlotFig where
    m >>= f =
        PlotFig $ \pf ->
            let !(pf', r) = doPlotFig m pf in doPlotFig (f $! r) $! pf'
    m >> m' = PlotFig
        $ \pf -> let !(pf', _) = doPlotFig m pf in doPlotFig m' $! pf'

