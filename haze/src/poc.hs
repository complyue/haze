
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}


import           UIO
import           Haze

import qualified RIO.Vector                    as V
import qualified RIO.Map                       as Map

import qualified Data.Vector.Fusion.Bundle     as VG
import qualified Data.Vector.Generic.New       as VG

import qualified Data.Aeson                    as A

default (Text, Int, Double)


ppoc :: UIO ()
ppoc = do
    let n = 20

    showPlot "g1" $ \pg -> do

        w1   <- openPlotWindow pg "w1"

        x    <- VG.runPrim $ VG.unstream $ VG.generate n fromIntegral
        ySin <- VG.runPrim $ VG.unstream $ VG.generate n $ sin . fromIntegral
        yCos <- VG.runPrim $ VG.unstream $ VG.generate n $ sin . fromIntegral
        ds   <- putDataSource w1
            $ Map.fromList [("x", x), ("sin", ySin), ("cos", yCos)]


        f1 <- addPlotFigure
            w1
            [ ("title"           , "SIN Figure")
            , ("toolbar_location", "above")
            , ("sizing_mode"     , "stretch_both")
            ]

        f1 $@ SetFigAttrs
            [ (["xaxis", "major_label_orientation"], LiteralValue (pi / 8.0))
            , (["xgrid", "grid_line_alpha"]        , LiteralValue 0.3)
            , (["ygrid", "grid_line_alpha"]        , LiteralValue 0.3)
            , ( ["xgrid", "major_label_overrides"]
              , LiteralValue $ (A.String . ("Pt#" <>) . tshow) <$> [1 .. n]
              )
            ]

        f1 $@ addGlyph
            "line"
            ds
            [ ("x"     , DataField "x")
            , ("y"     , DataField "sin")
            , ("color" , DataValue "#1122cc")
            , ("alpha" , DataValue 0.7)
            , ("legend", DataValue "SIN Curve")
            ]

        f2 <- addPlotFigure
            w1
            [ ("title"           , "COS Figure")
            , ("toolbar_location", "below")
            , ("sizing_mode"     , "stretch_both")
            ]

        f2 $@ SetFigAttrs
            [ (["xaxis", "major_label_orientation"], LiteralValue (pi / 8.0))
            , (["xgrid", "grid_line_alpha"]        , LiteralValue 0.3)
            , (["ygrid", "grid_line_alpha"]        , LiteralValue 0.3)
            , ( ["xgrid", "major_label_overrides"]
              , LiteralValue $ (A.String . ("Pt#" <>) . tshow) <$> [1 .. n]
              )
            ]

        f2 $@ addGlyph
            "line"
            ds
            [ ("x"     , DataField "x")
            , ("y"     , DataField "cos")
            , ("color" , DataValue "#11cc22")
            , ("alpha" , DataValue 0.7)
            , ("legend", DataValue "COS Curve")
            ]

        f1 $@ addLayout
            "Span"
            [ ("location"  , LiteralValue 0.0)
            , ("line_color", DataValue "blue")
            , ("line_alpha", LiteralValue 0.5)
            , ("dimension" , LiteralValue "width")
            , ("line_width", LiteralValue 1)
            ]
        f1 $@ addLayout
            "Span"
            [ ("location"  , LiteralValue 0.0)
            , ("line_color", DataValue "red")
            , ("line_alpha", LiteralValue 0.5)
            , ("dimension" , LiteralValue "height")
            , ("line_width", LiteralValue 1)
            ]

        f2 $@ SetFigAttrs
            [ ( ["yaxis", "ticker"]
              , newBokehObj
                  "BasicTicker"
                  [ ("min_interval", LiteralValue 0.01)
                  , ("max_interval", LiteralValue 5.0)
                  ]
              )
            ]

        f2 $@ SetGlyphAttrs
            "HoverTool"
            [ (["active"]  , LiteralValue False)
            , (["tooltips"], LiteralValue [["DataSeries", "$name"]])
            ]

        f2 $@ SetGlyphAttrs
            "Legend"
            [ (["location"]             , LiteralValue "top_left")
            , (["click_policy"]         , LiteralValue "hide")
            , (["background_fill_alpha"], LiteralValue 0.6)
            ]

        if True
            then do -- method 1
                _ <- linkAxes pg [(f1, "x_range"), (f2, "x_range")]
                _ <- linkAxes pg [(f1, "y_range"), (f2, "y_range")]
                pure ()
            else do -- method 2
                sharedX <- defineAxis pg
                sharedY <- defineAxis pg

                linkAxis f1 "x_range" sharedX
                linkAxis f1 "y_range" sharedY

                linkAxis f2 "x_range" sharedX
                linkAxis f2 "y_range" sharedY

        return ()
