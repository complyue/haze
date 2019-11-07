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

-- | Types shared among other modules are defined here to avoid cicular imports
--
-- You normally just import 'Haze' to use the DSL, and only need to import
-- this module for some reason you need sth not re-exported there.

module Haze.Types where

import           UIO

import qualified RIO.Map                       as Map
import qualified RIO.Vector.Storable           as VS

import qualified Data.Aeson                    as A

import qualified Data.ByteString.Builder       as BSB


-- * Types to name arguments & return values of API functions

type PlotGrpId = Text
type PlotWinId = Text

type ArgName = Text
type CtorName = Text
type MethodName = Text
type AttrName = Text
type RangeName = Text

newtype AxisRef = AxisRef Int
newtype WinRef = WinRef Int
newtype DataSourceRef = DataSourceRef Int
newtype FigRef = FigRef Int

-- | DataSource is at the core of Bokeh's data model
-- though other concrete types exist in Bokeh,
-- we only support CDS in Haze by far.
type ColumnDataSource = Map ColumnName ColumnData
type ColumnName = Text
-- | float64 is used uniformly for column data for now,
-- tho BokehJS can support more types, let's keep it simple.
--
-- note js within any browser inherently has no support of int64, 
-- is float32,int32,int16,int8 worth to be added beyond just float64?
type ColumnData = VS.Vector Double


-- * Literal & DSL yielded values to be compiled into JavaScript,
-- then to be further messing with BokehJS at browser site

data BokehExpr =
    -- | literal representation in JavaScript
    JsRepr Text
    -- | reference a column from associated 'ColumnDataSource', in form of:
    -- > {field: "ccc"}
    | DataField ColumnName
    -- | some BokehJS methods work better (or even only work) with arg value
    -- in form of:
    -- > {value: vvv}
    | DataValue BokehExpr
    -- | construct a BokehJS object in JavaScript with specified constructor and
    -- arguments, the 'CtorName' must resident within the `Bokeh` namespace
    | NewBokehObj CtorName [(ArgName, BokehExpr)]

bokehJsRepr :: BokehExpr -> Utf8Builder
bokehJsRepr = \case
    JsRepr    r -> display r
    DataField c -> "{field: " <> displayShow c <> "}"
    DataValue d -> "{value: " <> bokehJsRepr d <> "}"
    NewBokehObj ctor args ->
        "new bkh." <> display ctor <> "({\n" <> jsArgList args <> "})"

-- | not really helping, just provide the alternative to 'DataField'
-- yet with consistent capitalization schema as 'dataValue'
dataField :: ColumnName -> BokehExpr
dataField = DataField

-- | helper for 'DataValue' from values of type 'BokehValue'
dataValue :: BokehValue v => v -> BokehExpr
dataValue = DataValue . bokehExpr

-- | helper for 'NewBokehObj' from args with value type 'BokehValue'
newBokehObj :: BokehValue v => CtorName -> [(ArgName, v)] -> BokehExpr
newBokehObj ctor args =
    NewBokehObj ctor $ map (\(n, v) -> (n, bokehExpr v)) args

-- | literal `null` in JavaScript
jsNull :: BokehExpr
jsNull = JsRepr "null"


class BokehValue a where
    bokehExpr :: a -> BokehExpr

instance BokehValue BokehExpr where
    bokehExpr = id

-- | this enables the simulation of heterogenous lists of 'BokehValue's, like:
--
-- > [ bokehExpr x, bokehExpr y, ...]
instance BokehValue [BokehExpr] where
    bokehExpr l =
        JsRepr
            $  utf8BuilderToText
            $  (foldl' (\b i -> b <> bokehJsRepr i <> ", ") "[ " l)
            <> "]"

-- | this enables value expressions in Haskell to be written without any boxing,
-- where a 'BokehValue' is needed, as long as the value has a type implementing
-- 'A.ToJSON'
instance {-# OVERLAPPABLE #-} (A.ToJSON j) => BokehValue j where
    bokehExpr = JsRepr . utf8BuilderToText . jsStringify

instance BokehValue DataSourceRef where
    bokehExpr r =
        JsRepr
            $  utf8BuilderToText
            $  "cdsa["
            <> displayShow (let DataSourceRef d = r in d)
            <> "]"

instance BokehValue FigRef where
    bokehExpr r =
        JsRepr
            $  utf8BuilderToText
            $  "figs["
            <> displayShow (let FigRef d = r in d)
            <> "]"

-- | map a homogeneous list of 'BokehValue' to an array of the values into JavaScript
--
-- to simulate a heterogeneous list, use:
--
-- > [ bokehExpr x, bokehExpr y, ...]
instance BokehValue v => BokehValue [v] where
    bokehExpr l =
        JsRepr
            $  utf8BuilderToText
            $  (foldl' (\b i -> b <> (bokehJsRepr $bokehExpr i) <> ", ") "[ " l)
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


-- * ADTs to collect the plotting specs from DSL

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

