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

module Haze
    ( module Haze.DSL
    -- helpers
    , generateSeries
    , iterateSeries
    )
where

import           UIO

import qualified RIO.Vector.Storable           as VS

import           Haze.Types
import           Haze.DSL


-- | helpers on data generation

generateSeries :: MonadIO m => Int -> (Int -> Double) -> m ColumnData
generateSeries n g = liftIO $ VS.thaw $ VS.generate n g

iterateSeries
    :: MonadIO m => Int -> (Double -> Double) -> Double -> m ColumnData
iterateSeries n f x = liftIO $ VS.thaw $ VS.iterateN n f x

