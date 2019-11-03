{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Haze.Plot
    ( uiPlot
    )
where

import           UIO
import           HaduiUtil

import qualified RIO.Text                      as T
import           Text.Printf
import qualified Data.ByteString.Builder       as BSB

import qualified RIO.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable  as VM
import qualified Data.Vector.Fusion.Bundle     as VG
import qualified Data.Vector.Generic.New       as VG

import           Foreign

import qualified Data.Map                      as Map

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Network.WebSockets            as WS

import           Haze.Types


outputBokehValue :: BokehValue -> Utf8Builder
outputBokehValue = \case
    LiteralValue v -> json2utf8 v
    DataField    f -> json2utf8 [aesonQQ|{
field: #{f}
}|]
    DataValue v -> json2utf8 [aesonQQ|{
field: #{v}
}|]
    NewBokehObj ctor args ->
        "Bokeh." <> display ctor <> "(" <> outputBokehArgs args <> ")"
  where
    json2utf8 :: A.ToJSON a => a -> Utf8Builder
    json2utf8 = Utf8Builder . BSB.lazyByteString . A.encode

outputBokehArgs :: [(ArgName, BokehValue)] -> Utf8Builder
outputBokehArgs m = (foldl' kv "{" m) <> "}"
    where kv p (k, v) = p <> display k <> ": " <> outputBokehValue v <> ", "

outputBokehDict :: Map ArgName BokehValue -> Utf8Builder
outputBokehDict m = outputBokehArgs $ Map.assocs m


totalDataSize :: PlotGroup -> UIO Double
totalDataSize pg = do
    pws <- readIORef $ windowsInGroup pg
    tdl <- foldM cumWin (0 :: Int64) pws
    return $ fromIntegral tdl * fromIntegral (sizeOf (0 :: Double))
  where
    cumWin ds pw = foldlDeque cumCDS ds (dsInWindow pw)
    cumCDS ds cds = foldM cumCol ds $ Map.elems cds
    cumCol ds cd = return $ ds + (fromIntegral $ VM.length cd)


uiPlot :: GroupId -> (PlotGroup -> UIO ()) -> UIO ()
uiPlot grpId plotProcedure =
    (ask >>= \uio ->
        let gil = haduiGIL uio
        in
            isEmptyMVar gil >>= \case
                True ->
                    logError
                        $  display
                        $  "No ws in context to plot group "
                        <> grpId
                False -> readMVar gil >>= plotViaWS
    )
  where
    plotViaWS wsc = do
        let uiMsg msg = wsSendText
                wsc
                [aesonQQ|{
"type": "msg"
, "msgText": #{msg}
}|]

        -- prepare the plot state
        nla_ <- newIORef 0
        pws_ <- newIORef []
        let pg = PlotGroup { plotGrpId       = grpId
                           , numOfLinkedAxes = nla_
                           , windowsInGroup  = pws_
                           }
        -- realize the procedure which uses the DSL to do plot setup
        plotProcedure pg
        -- report total data size to UI, get the user some intuition
        -- for how long he/she should expect before all rendered
        tds <- totalDataSize pg
        uiMsg
            $  "total plot data size: "
            <> (T.pack $ printf "%0.1f" (tds / 1024 / 1024))
            <> " MB"
        -- interpret the stated resulted from DSL, by sending json
        -- cmds and binary column data to UI
        outputPlot wsc pg


outputPlot :: WS.Connection -> PlotGroup -> UIO ()
outputPlot wsc pg = do
    -- nAxes <- readIORef $ numOfLinkedAxes pg
    pws <- readIORef (windowsInGroup pg)
    for_ pws $ outputWin wsc

--     let plotCode = [text|
-- console.log('here it is!')
-- |]
--     wsSendText
--         wsc
--         [aesonQQ|{
-- "type": "call"
-- , "name": "plotGroup"
-- , "args": [#{plotCode}]
-- }|]


outputWin :: WS.Connection -> PlotWindow -> UIO ()
outputWin wsc pw = do
    -- send binary packets for column data, return cumulated column name list
    cnl <- foldrDeque processCDSQ [] dsiw
    -- generate plot code for all figures into a 'Utf8Builder'
    pcb <- foldM outputFigure mempty =<< (readIORef $ figuresInWindow pw)
    let !plotCode =
            utf8BuilderToText
                $  "async function(pgid, pwid, cdsa) {\n\n"
    -- full plot code of a window is wrapped in a functon like this
                <> pcb
                <> "\n}\n"
    wsSendText
        wsc
        [aesonQQ|{
"type": "call"
, "name": "plotWin"
, "args": [#{pgid}, #{pwid}, #{cnl}, #{plotCode}]
}|]

  where
    !dsiw = dsInWindow pw
    !pwid = plotWinId pw
    !pg   = plotGroup pw
    !pgid = plotGrpId pg

    processCDSQ :: ColumnDataSource -> [[Text]] -> UIO [[Text]]
    -- column names are listed in cds/col order,
    -- each column has its binary data sent as one ws packet,
    -- but columns are sent in revered cds/col order, so at js site
    -- each column is poped from the stack of received packets.
    processCDSQ cds cnl = do
        for_ (reverse $ Map.elems cds) $ \cd -> wsSendData wsc cd
        return $ Map.keys cds : cnl


outputFigure :: Utf8Builder -> PlotFigure -> UIO Utf8Builder
outputFigure pcb pf = do
    let fcb0 = pcb <> "(async function(fig) {\n"
    fcb1 <- foldM outputFigOp fcb0 =<< (readIORef $ figureOps pf)
    fcb2 <-
        (foldM outputAxisLink fcb1) . Map.assocs =<< (readIORef $ linkedAxes pf)
    figArgs <- readIORef $ figureArgs pf
    let fcb3  = fcb2 <> "})(\n"
        fArgs = outputBokehDict figArgs
    return $ fcb3 <> fArgs <> ")\n"


outputFigOp :: Utf8Builder -> FigureOp -> UIO Utf8Builder
outputFigOp fcb op = do

    return $ fcb <> "xxx\n"


outputAxisLink :: Utf8Builder -> (RangeName, AxisRef) -> UIO Utf8Builder
outputAxisLink fcb (rng, axis) = do

    return $ fcb <> "xxx\n"


