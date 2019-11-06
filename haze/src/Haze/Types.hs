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

import qualified RIO.Map                       as Map
import qualified RIO.Vector.Storable           as VS

import qualified Data.Aeson                    as A

import qualified Data.ByteString.Builder       as BSB


-- Types to name arguments & return values of API functions

type PlotGrpId = Text
type PlotWinId = Text

type ArgName = Text
type CtorName = Text
type MethodName = Text
type AttrName = Text
type RangeName = Text

newtype AxisRef = AxisRef Int  deriving (Eq, Ord, Read, Show)
newtype WinRef = WinRef Int  deriving (Eq, Ord, Read, Show)
newtype DataSourceRef = DataSourceRef Int  deriving (Eq, Ord, Read, Show)
newtype FigRef = FigRef Int  deriving (Eq, Ord, Read, Show)

-- | DataSource is at the core of Bokeh's data model
-- though other concrete types exist in Bokeh,
-- we only support CDS in Haze by far.
type ColumnDataSource = Map ColumnName ColumnData
type ColumnName = Text
-- | float64 is used uniformly for column data for now,
-- tho BokehJS can support more types, let's keep it simple.
-- note js within any browser inherently has no support of int64, 
-- float32/int32/int16/int8 worth to be added beyond just float64?
type ColumnData = VS.MVector (PrimState IO) Double

-- | reference a column from associated 'ColumnDataSource', in form of:
-- >>> {field: 'ccc'}
newtype DataField = DataField Text  deriving (Eq, Ord, Read, Show)
instance BokehValue DataField where
    bokehExpr field =
        BokehExpr
            $  "{field: "
            <> displayShow (let DataField n = field in n)
            <> "}"

-- | some BokehJS methods work better (or even only work) with arg value
-- in form of:
-- >>> {value: vvv}
data DataValue v where
    DataValue ::BokehValue v => v -> DataValue v
instance BokehValue v => BokehValue (DataValue v) where
    bokehExpr value =
        BokehExpr
            $  "{value: "
            <> (bokehJsRepr . bokehExpr) (let DataValue d = value in d)
            <> "}"

-- | construct a BokehJS object in JavaScript with specified constructor and
-- arguments, the 'CtorName' must resident within the `Bokeh` namespace
data NewBokehObj v where
    NewBokehObj ::BokehValue v=> CtorName -> [(ArgName, v)] -> NewBokehObj v
instance BokehValue v => BokehValue (NewBokehObj v) where
    bokehExpr c =
        let NewBokehObj ctor args = c
        in  BokehExpr
                $  "new bkh."
                <> display ctor
                <> "({\n"
                <> jsArgList args
                <> "})"

-- | literal `null` in JavaScript
jsNull :: BokehExpr
jsNull = BokehExpr "null"


-- literal & DSL yielded values to be compiled into JavaScript,
-- then to be further messing with BokehJS at browser site

newtype BokehExpr = BokehExpr { bokehJsRepr :: Utf8Builder }

class BokehValue a where
    bokehExpr :: a -> BokehExpr

instance BokehValue BokehExpr where
    bokehExpr = id

-- | this enables the simulation of heterogenous lists of 'BokehValue's, like:
-- >>> [bokehExpr x, bokehExpr y, ...]
instance BokehValue [BokehExpr] where
    bokehExpr l =
        BokehExpr $ (foldl' (\b i -> b <> bokehJsRepr i <> ", ") "[" l) <> "]"

-- | this enables value expressions in Haskell to be written without any boxing,
-- where a 'BokehValue' is needed, as long as the value has a type implementing
-- 'A.ToJSON'
instance {-# OVERLAPPABLE #-} (A.ToJSON j) => BokehValue j where
    bokehExpr = BokehExpr . jsStringify

instance BokehValue DataSourceRef where
    bokehExpr r = BokehExpr $ "cdsa[" <> displayShow r <> "]"

instance BokehValue FigRef where
    bokehExpr r = BokehExpr $ "figs[" <> displayShow r <> "]"

-- | map a homogeneous list of 'BokehValue' to an array of the values into JavaScript
--
-- to simulate a heterogeneous list, use:
-- >>> [bokehExpr x, bokehExpr y, ...]
instance BokehValue v => BokehValue [v] where
    bokehExpr l =
        BokehExpr
            $  (foldl' (\b i -> b <> (bokehJsRepr $bokehExpr i) <> ", ") "[" l)
            <> "]"


-- | similar to `JSON.stringify` in JavaScript
jsStringify :: A.ToJSON a => a -> Utf8Builder
jsStringify = Utf8Builder . BSB.lazyByteString . A.encode

-- | convert the pairs of argument name/value to JavaScript representation
jsArgList :: BokehValue v => [(ArgName, v)] -> Utf8Builder
jsArgList args = foldl' cata "" args
  where
    cata b (n, x) =
        b <> "  " <> display n <> ": " <> (bokehJsRepr $ bokehExpr x) <> ",\n"

-- | convert the map of argument name/value to JavaScript representation
jsArgMap :: Map ArgName BokehExpr -> Utf8Builder
jsArgMap d = jsArgList $ Map.assocs d


-- ADTs to collect the plotting specs from DSL

data PltGrp = PltGrp {
    pgId :: Text
    , numSyncAxes :: Int
    , pltWins :: [PltWin]
}

data PltWin = PltWin {
    pwId :: Text
    , colDataSrcs :: [ColumnDataSource]
    , pltFigs :: [PltFig]
    , pltLays :: [PltLay]
}

data PltFig = PltFig {
    figArgs :: Map ArgName BokehExpr
    , figOps :: [FigOp]
    , figLinks :: Map RangeName AxisRef
}

data FigOp =
      SetFigAttrs [([AttrName], BokehExpr)]
    | AddGlyph MethodName DataSourceRef (Map ArgName BokehExpr)
    | AddLayout CtorName (Map ArgName BokehExpr)
    | SetGlyphAttrs CtorName [([AttrName], BokehExpr)]

data PltLay = PltLay {
    layMethod :: MethodName
    , layChildren :: BokehExpr
    , layOpts :: Map ArgName BokehExpr
    , layTarget :: BokehExpr
}

