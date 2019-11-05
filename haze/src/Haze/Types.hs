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

module Haze.Types where

import           UIO

import qualified RIO.Vector.Storable           as VS

import qualified Data.Aeson                    as A

import qualified Data.ByteString.Builder       as BSB


stringify :: A.ToJSON a => a -> Utf8Builder
stringify = Utf8Builder . BSB.lazyByteString . A.encode


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


data PG = PG {
    pgId :: Text
    , lxs :: Int
    , pws :: [PW]
}

data PW = PW {
    pwId :: Text
    , cdss :: [ColumnDataSource]
    , figs :: [Fig]
    , lays :: [Lay]
}

data Fig = Fig {
    figArgs :: Map ArgName BV
    , figOps :: [FigOp]
    , figLinks :: Map RangeName AxisRef
}

data FigOp =
      SetFigAttrs' [([AttrName], BV)]
    | AddGlyph' MethodName
    -- TODO cont.

data Lay = Lay {
    layMethod :: MethodName
    , layChildren :: BV
    , layOpts :: Map ArgName BV
    , layTarget :: Text
}


newtype BV = BV { bvJsRepr :: Utf8Builder }

class BVC a where
    toBV :: a -> BV

instance (A.ToJSON j) => BVC j where
    toBV = BV . stringify


