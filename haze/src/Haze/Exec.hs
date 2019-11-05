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

import           Data.Foldable                  ( foldrM )

import qualified RIO.Text                      as T
import           Text.Printf

import qualified Data.Vector.Storable.Mutable  as VM

import           Foreign

import qualified Data.Map                      as Map

import           Data.Aeson.QQ                  ( aesonQQ )

import qualified Network.WebSockets            as WS

import           Haze.Types


compileBokehValue :: BokehValue -> Utf8Builder
compileBokehValue = \case
    LiteralValue v -> stringify v
    DataField    f -> stringify [aesonQQ|{
field: #{f}
}|]
    DataValue v -> stringify [aesonQQ|{
value: #{v}
}|]
    NewBokehObj ctor args ->
        "new Bokeh."
            <> display ctor
            <> "({\n"
            <> compileBokehArgs args
            <> "})\n"
    JsNull -> "null"
    JsArray jsa ->
        (foldl' (\b v -> b <> compileBokehValue v <> ", ") "[ " jsa) <> "]"
    JsFigure fig -> "fig" <> (display $ figureNo fig)

compileBokehArgs :: [(ArgName, BokehValue)] -> Utf8Builder
compileBokehArgs m = (foldl' nv "" m) <> ""
  where
    nv p (n, v) =
        p <> "  " <> display n <> ": " <> compileBokehValue v <> ",\n"

compileBokehDict :: Map ArgName BokehValue -> Utf8Builder
compileBokehDict m = compileBokehArgs $ Map.assocs m


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


outputWin :: WS.Connection -> PlotWindow -> UIO ()
outputWin wsc pw = do
    -- send binary packets for column data, return cumulated column name list
    cnl  <- foldrDeque processCDSQ [] dsiw
    -- generate plot code for all figures into a 'Utf8Builder'
    pcb0 <- foldrM outputFigure mempty =<< (readIORef $ figuresInWindow pw)
    -- generate layout code, append to plot code builder
    pls  <- readIORef $ plotLayouts pw
    let pcb1 = foldr compileLayout pcb0 $ pls
    -- send the data to browser for actual plotting
    let !plotCode =
            utf8BuilderToText
            -- full plot code of a window is wrapped in a functon like this
                $  "(pgid, pwid, cdsa)=>{\nconst bkh=Bokeh, plt=bkh.Plotting;\n"
                -- <> "debugger; \n"
                <> pcb1
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


compileLayout :: PlotLayout -> Utf8Builder -> Utf8Builder
compileLayout pl pcb =
    pcb
        -- <> "debugger;\n"
        <> "plt.show(plt."
        <> (display $ layoutMethod pl)
        <> "("
        <> (compileBokehValue $ layoutChildren pl)
        <> ", {\n"
        <> (compileBokehDict $ layoutOptions pl)
        <> "}), "
        <> (display $ layoutTarget pl)
        <> ");\n"


outputFigure :: PlotFigure -> Utf8Builder -> UIO Utf8Builder
outputFigure pf pcb = do
    let figNo = figureNo pf
    figArgs <- readIORef $ figureArgs pf
    let fcb0 =
            pcb -- <> "debugger;\n"
                <> "const fig"
                <> display figNo
                <> " = plt.figure({\n"
                <> compileBokehDict figArgs
                <> "});\nvar fig = fig"
                <> display figNo
                <> ";\n"

    fops <- readIORef $ figureOps pf
    let fcb1 = foldr compileFigOp fcb0 fops
    laxs <- readIORef $ linkedAxes pf
    let fcb2 = foldr (compileAxisLink pgid) fcb1 $ Map.assocs laxs

    return fcb2
  where
    !pw   = plotWindow pf
    !pg   = plotGroup pw
    !pgid = plotGrpId pg


compileFigOp :: FigureOp -> Utf8Builder -> Utf8Builder
compileFigOp fop fcb = case fop of

    AddGlyph mth ds args ->
        fcb
            <> "fig."
            <> display mth
            <> "({\n  source: cdsa["
            <> display ds
            <> "],\n"
            <> compileBokehArgs args
            <> "})\n"

    AddLayout ctor args ->
        fcb
            <> "fig.add_layout(new Bokeh."
            <> display ctor
            <> "({\n"
            <> compileBokehArgs args
            <> "}))\n"

    SetGlyphAttrs ctor sas -> foldr setGlyAttr fcb1 sas <> "}\n"
      where
        fcb1 =
            fcb <> "for (let g of fig.select(Bokeh." <> display ctor <> ")) {\n"

    SetFigAttrs sas -> foldr setFigAttr fcb sas

  where

    setFigAttr (path, val) cb =
        cb
            <> foldl' (\b p -> b <> "." <> display p) "fig" path
            <> " = "
            <> compileBokehValue val
            <> "\n"

    setGlyAttr (path, val) cb =
        cb
            <> foldl' (\b p -> b <> "." <> display p) "g" path
            <> " = "
            <> compileBokehValue val
            <> "\n"


compileAxisLink :: GroupId -> (RangeName, AxisRef) -> Utf8Builder -> Utf8Builder
compileAxisLink pgid (rng, axis) fcb =
    fcb
        <> "syncRange(fig."
        <> display rng
        <> ", 'rng@"
        <> display pgid
        <> "#"
        <> display axis
        <> "');\n"
