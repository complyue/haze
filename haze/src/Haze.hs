{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

-- | Haze defines an embedded DSL to do low level plotting with
-- BokehJS in the browser as visualization target.

module Haze
    (
    -- * The plotting DSL    
      module Haze.DSL
    -- * Helpers on data manipulation
    , generateSeries
    , iterateSeries
    , mapSeries
    , seriesLength
    )
where

import           UIO

import qualified RIO.Vector.Storable           as VS

import           Haze.Types
import           Haze.DSL


generateSeries :: Int -> (Int -> Double) -> ColumnData
generateSeries n g = VS.generate n g

iterateSeries :: Int -> (Double -> Double) -> Double -> ColumnData
iterateSeries n f x = VS.iterateN n f x

mapSeries :: (Double -> Double) -> ColumnData -> ColumnData
mapSeries f s = VS.map f s

seriesLength :: ColumnData -> Int
seriesLength = VS.length

