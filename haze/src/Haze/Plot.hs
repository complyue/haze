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

import qualified RIO.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable  as VM
import qualified Data.Vector.Fusion.Bundle     as VG
import qualified Data.Vector.Generic.New       as VG

import           Foreign

import qualified Data.Map                      as Map

import qualified Data.Aeson                    as A
import           Data.Aeson.QQ                  ( aesonQQ )
import           NeatInterpolation

import qualified Network.WebSockets            as WS

import           Haze.Types


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

        -- Haskell code use the DSL to do plot setup
        plotProcedure pg

        -- all windows defined
        pws <- readIORef (windowsInGroup pg)

        -- report total data size to UI, get the user some intuition
        -- for how long he/she should expect before all rendered
        tds <- totalDataSize pws
        uiMsg
            $  "total plot data size: "
            <> (T.pack $ printf "%0.1f" (tds / 1024 / 1024))
            <> " MB"

        -- send json cmds and binary column data to UI
        outputPlot wsc pg pws


outputPlot :: WS.Connection -> PlotGroup -> [PlotWindow] -> UIO ()
outputPlot wsc pg pws = do
    forM_ pws sendDiW

    let plotCode = [text|
console.log('here it is!')
|]
    wsSendText
        wsc
        [aesonQQ|{
"type": "call"
, "name": "plot"
, "args": [#{plotCode}]
}|]
  where
    forDeque q f = foldlDeque (const f) () q
    sendDiW pw = forDeque (dsInWindow pw) $ \cds -> do
        for_ (Map.elems cds) $ \cd -> wsSendData wsc cd
        -- 
        wsSendText
            wsc
            [aesonQQ|{
"type": "call"
, "name": "plotWin"
, "args": []
}|]


totalDataSize :: [PlotWindow] -> UIO Double
totalDataSize pws = do
    tdl <- foldM cumWin (0 :: Int64) pws
    return $ fromIntegral tdl * fromIntegral (sizeOf (0 :: Double))
  where
    cumWin ds pw = foldlDeque cumCDS ds (dsInWindow pw)
    cumCDS ds cds = foldM cumCol ds $ Map.elems cds
    cumCol ds cd = return $ ds + (fromIntegral $ VM.length cd)
