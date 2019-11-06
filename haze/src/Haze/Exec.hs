{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

-- | The Haze executor:
--   * transfers binary data to js site (the browser);
--   * transpiles the plotting DSL (in Haskell) to Javascript,
--     then send to js site (the browser) for execution.

module Haze.Exec
    ( uiPlot
    )
where

import           UIO
import           HaduiUtil

import qualified RIO.Text                      as T
import           Text.Printf

import qualified Data.Vector.Storable.Mutable  as VM

import           Foreign

import qualified Data.Map                      as Map

import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Network.WebSockets            as WS

import           Haze.Types
import           Haze.Monad


-- | do a plot over Hadui websocket to browser
uiPlot
    :: PlotGrpId -- ^ a browser window identified by 'PlotGrpId' + 'PlotWinId'
                 -- is reused across multiple plot works
    -> Plot a -- ^ specify windows and figures to plot with this monadic
              -- computation
    -> UIO a
uiPlot grpId plotAct =
    (ask >>= \uio ->
        let
            !gil    = haduiGIL uio
            (pg, x) = doPlot
                plotAct
                PltGrp { pgId = grpId, numSyncAxes = 0, pltWins = [] }
        in
            isEmptyMVar gil >>= \case
                True -> do
                    logError
                        $  display
                        $  "No ws in context to plot group "
                        <> grpId
                    return x
                False -> readMVar gil >>= (plotToUI $! pg) x
    )
  where
    plotToUI :: PltGrp -> a -> WS.Connection -> UIO a
    plotToUI pg x wsc = do
        let uiMsg :: Text -> UIO ()
            uiMsg msg = wsSendText
                wsc
                [aesonQQ|{
"type": "msg"
, "msgText": #{msg}
}|]

        -- report total data size to UI, get the user some intuition for
        -- how long he/she should wait before all data visually rendered
        let (nWins, nFigs, tds) = statsPlot pg
        uiMsg
            $  utf8BuilderToText
            $  "Plotting total "
            <> display nFigs
            <> " figure(s) into "
            <> display nWins
            <> " window(s) with "
            <> (display $ T.pack $ printf "%0.1f" $ tds / 1024 / 1024)
            <> " MB data."

        -- interpret the stated resulted from DSL, by sending json
        -- cmds and binary column data to browser
        traverse_ (sendWindow grpId wsc) $ reverse $ pltWins pg

        return x


sendWindow :: PlotGrpId -> WS.Connection -> PltWin -> UIO ()
sendWindow pgid wsc pw = do
    -- send binary packets for column data, return cumulated column name list
    cnl <- foldM processCDS [] $ colDataSrcs pw
    -- full plot code of a window is wrapped in a js functon like this
    let !pcb0 =
            "(pgid, pwid, cdsa)=>{\nconst bkh=Bokeh, plt=bkh.Plotting;\n"
                -- <> "debugger; \n"
                <> "  document.title = ''+pgid+'#'+pwid;\n"
                <> "  const figs = [];\n"
    -- generate plot code for all figures into a 'Utf8Builder'
    let !pcb1     = foldr (compileFigure pgid) pcb0 $ pltFigs pw
-- generate layout code, append to plot code builder
        !pcb2     = foldr compileLayout pcb1 $ pltLays pw
-- send the data to browser for actual plotting
        !plotCode = utf8BuilderToText $ pcb2 <> "\n}\n"
    wsSendText
        wsc
        [aesonQQ|{
"type": "call"
, "name": "plotWin"
, "args": [#{pgid}, #{pwId pw}, #{cnl}, #{plotCode}]
}|]

  where
    processCDS cnl cds = do
        traverse_ (wsSendData wsc) $ reverse $ Map.elems cds
        return $ Map.keys cds : cnl


compileFigure :: PlotGrpId -> PltFig -> Utf8Builder -> Utf8Builder
compileFigure pgid pf pcb =
    let pcb0 =
                pcb
                    <> "debugger;\n"
                    <> "var fig = plt.figure({\n"
                    <> jsArgMap (figArgs pf)
                    <> "});\nfigs.push(fig);\n"
        pcb1 = foldr compileFigOp pcb0 $ figOps pf
        pcb2 = foldl' (compileAxisLink pgid) pcb1 $ Map.assocs $ figLinks pf
    in  pcb2

compileFigOp :: FigOp -> Utf8Builder -> Utf8Builder
compileFigOp fop pcb = case fop of

    AddGlyph mth dsr args ->
        pcb
            <> "fig."
            <> display mth
            <> "({\n  source: cdsa["
            <> display (let DataSourceRef r = dsr in r)
            <> "],\n"
            <> jsArgMap args
            <> "})\n"

    AddLayout ctor args ->
        pcb
            <> "fig.add_layout(new bkh."
            <> display ctor
            <> "({\n"
            <> jsArgMap args
            <> "}))\n"

    SetGlyphAttrs ctor sas -> foldr setGlyAttr pcb1 sas <> "}\n"
      where
        pcb1 =
            pcb <> "for (let g of fig.select(bkh." <> display ctor <> ")) {\n"

    SetFigAttrs sas -> foldr setFigAttr pcb sas

  where

    setFigAttr (path, val) cb =
        cb
            <> foldl' (\b p -> b <> "." <> display p) "fig" path
            <> " = "
            <> bokehJsRepr val
            <> "\n"

    setGlyAttr (path, val) cb =
        cb
            <> foldl' (\b p -> b <> "." <> display p) "g" path
            <> " = "
            <> bokehJsRepr val
            <> "\n"

compileAxisLink
    :: PlotGrpId -> Utf8Builder -> (RangeName, AxisRef) -> Utf8Builder
compileAxisLink pgid pcb (rng, axis) =
    pcb
        <> "syncRange(fig."
        <> display rng
        <> ", 'rng@"
        <> display pgid
        <> "#"
        <> display (let AxisRef r = axis in r)
        <> "');\n"


compileLayout :: PltLay -> Utf8Builder -> Utf8Builder
compileLayout pl pcb =
    pcb
        -- <> "debugger;\n"
        <> "plt.show(plt."
        <> (display $ layMethod pl)
        <> "("
        <> (bokehJsRepr $ layChildren pl)
        <> ", {\n"
        <> (jsArgMap $ layOpts pl)
        <> "}), "
        <> (bokehJsRepr $ layTarget pl)
        <> ");\n"


statsPlot :: PltGrp -> (Int, Int, Double)
statsPlot pg =
    (nWins, nFigs, fromIntegral tdl * fromIntegral (sizeOf (0 :: Double)))
  where
    (nWins, nFigs, tdl) =
        foldl' cumWin (0 :: Int, 0 :: Int, 0 :: Int64) $ pltWins pg
    cumWin (nw, nf, dl) pw =
        foldl' cumCDS (nw + 1, nf + length (pltFigs pw), dl) $ colDataSrcs pw
    cumCDS (nw, nf, dl) cds = foldl' cumCol (nw, nf, dl) $ Map.elems cds
    cumCol (nw, nf, dl) cd = (nw, nf, dl + (fromIntegral $ VM.length cd))

