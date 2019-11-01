{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Haze
    ( module Haze.DSL
    -- helpers
    , generateSeries
    )
where

import           UIO
import qualified Data.Vector.Fusion.Bundle     as VG
import qualified Data.Vector.Generic.New       as VG

import           Haze.Types
import           Haze.DSL


-- | helper on data generation
generateSeries :: MonadIO m => Int -> (Int -> Double) -> m ColumnData
generateSeries n g = liftIO $ VG.runPrim $ VG.unstream $ VG.generate n g

