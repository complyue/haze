
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


default (Text, Int, Double)


ppoc :: UIO ()
ppoc = do
    -- data parameter(s)
    let n = 20
    -- crunch for data
    x    <- generateSeries n fromIntegral
    ySin <- generateSeries n $ sin . fromIntegral
    yCos <- generateSeries n $ cos . fromIntegral
    -- do plot
    uiPlot "g1" $ do
        -- define axes to be synchronized
        sharedX <- defineAxis
        sharedY <- defineAxis
        -- plot in separate windows
        void $ openWindow "w1" $ do
            -- each window must have its own data source
            ds <- putDataSource [("x", x), ("sin", ySin), ("cos", yCos)]
            -- each window can have one or more figures
            f1 <-
                figure
                        [ ("title"           , bokehExpr "SIN Figure")
                        , ("toolbar_location", bokehExpr "right")
                        , ( "tools"
                          , bokehExpr
                              [ "crosshair"
                              , "pan"
                              , "xwheel_zoom"
                              , "ywheel_zoom"
                              , "box_zoom"
                              , "hover"
                              , "undo"
                              , "redo"
                              , "reset"
                              ]
                          )
                        , ("sizing_mode", bokehExpr "stretch_both")
                        ]
                    $ do
                          setFigAttrs
                              [ ( ["xaxis", "major_label_orientation"]
                                , JsRepr "Math.PI/8"
                                )
                              , (["xgrid", "grid_line_alpha"], bokehExpr 0.3)
                              , (["ygrid", "grid_line_alpha"], bokehExpr 0.3)
                              , ( ["xgrid", "major_label_overrides"]
                                , bokehExpr
                                $   (("N#" <>) . tshow)
                                <$> [0 .. n - 1]
                                )
                              ]
                          addGlyph
                              "line"
                              ds
                              [ ("x"     , dataField "x")
                              , ("y"     , dataField "sin")
                              , ("color" , dataValue "#1122cc")
                              , ("alpha" , dataValue 0.7)
                              , ("legend", dataValue "SIN Curve")
                              ]
                          addLayout
                              "Span"
                              [ ("location"  , bokehExpr 0.0)
                              , ("line_color", dataValue "blue")
                              , ("line_alpha", bokehExpr 0.5)
                              , ("dimension" , bokehExpr "width")
                              , ("line_width", bokehExpr 1)
                              ]
                          addLayout
                              "Span"
                              [ ("location"  , bokehExpr 0.0)
                              , ("line_color", dataValue "red")
                              , ("line_alpha", bokehExpr 0.5)
                              , ("dimension" , bokehExpr "height")
                              , ("line_width", bokehExpr 1)
                              ]
                          linkFigAxis "x_range" sharedX
                          linkFigAxis "y_range" sharedY

            f2 <-
                figure
                        [ ("title"           , bokehExpr "COS Figure")
                        , ("toolbar_location", bokehExpr "right")
                        , ( "tools"
                          , bokehExpr
                              [ "crosshair"
                              , "pan"
                              , "xwheel_zoom"
                              , "ywheel_zoom"
                              , "box_zoom"
                              , "hover"
                              , "undo"
                              , "redo"
                              , "reset"
                              ]
                          )
                        , ("sizing_mode", bokehExpr "stretch_both")
                        ]
                    $ do
                          setFigAttrs
                              [ ( ["xaxis", "major_label_orientation"]
                                , JsRepr "Math.PI/8"
                                )
                              , (["xgrid", "grid_line_alpha"], bokehExpr 0.3)
                              , (["ygrid", "grid_line_alpha"], bokehExpr 0.3)
                              , ( ["xgrid", "major_label_overrides"]
                                , bokehExpr
                                $   (("N#" <>) . tshow)
                                <$> [0 .. n - 1]
                                )
                              , ( ["yaxis", "ticker"]
                                , NewBokehObj
                                    "BasicTicker"
                                    [ ("min_interval", bokehExpr 0.01)
                                    , ("max_interval", bokehExpr 5.0)
                                    ]
                                )
                              ]
                          addGlyph
                              "line"
                              ds
                              [ ("x"     , dataField "x")
                              , ("y"     , dataField "cos")
                              , ("color" , dataValue "#11cc22")
                              , ("alpha" , dataValue 0.7)
                              , ("legend", dataValue "COS Curve")
                              ]
                          setGlyphAttrs
                              "HoverTool"
                              [ (["active"], bokehExpr False)
                              , ( ["tooltips"]
                                , bokehExpr [["DataSeries", "$name"]]
                                )
                              ]
                          setGlyphAttrs
                              "Legend"
                              [ (["location"], bokehExpr "top_left")
                              , (["click_policy"]         , bokehExpr "hide")
                              , (["background_fill_alpha"], bokehExpr 0.6)
                              ]
                          linkFigAxis "x_range" sharedX
                          linkFigAxis "y_range" sharedY

            showPlot
                "gridplot"
                [[f1], [f2]]
                [ ("merge_tools", bokehExpr False)
                , ("sizing_mode", bokehExpr "stretch_both")
                ]
                jsNull -- null targets the plot to html body element

