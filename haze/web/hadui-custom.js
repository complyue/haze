/**
 * hadui-custom.js
 */

import { HazeWSC } from "/haze.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `let

  -- data crunching algorithm, normally crafted in a module file
  -- within the project underhood
  prepareData :: UIO (ColumnData, ColumnData, ColumnData)
  prepareData = do
      -- data parameter(s)
      let n = 500
      -- crunch for data
      let x =
              generateSeries n $ \\i ->
                  8.0 * pi * (fromIntegral i) / (fromIntegral $ n - 1)
          ySin = mapSeries sin x
          yCos = mapSeries cos x
      return (x, ySin, yCos)

  -- the plot specification, normally crafted in a module file
  -- within the project underhood
  doPlot :: (ColumnData, ColumnData, ColumnData) -> Plot ()
  doPlot (x, ySin, yCos) = do
        -- define axes to be synchronized
      sharedX <- defineAxis
      sharedY <- defineAxis
      -- plot in separate windows
      void $ openWindow "Win1" $ do
          -- each window must have its own data source
          ds <- putDataSource [("x", x), ("sin", ySin), ("cos", yCos)]
          -- each window can have one or more figures
          f1 <-
              figure
                      [ ("title"           , bokehExpr "SIN Figure")
                      , ("toolbar_location", bokehExpr "left")
                      , ("sizing_mode"     , bokehExpr "stretch_both")
                      ]
                  $ do
                        addGlyph
                            "line"
                            ds
                            [ ("x"     , dataField "x")
                            , ("y"     , dataField "sin")
                            , ("color" , dataValue "#1122cc")
                            , ("alpha" , dataValue 0.7)
                            , ("legend", dataValue "SIN Curve")
                            ]
                        linkFigAxis "x_range" sharedX
                        linkFigAxis "y_range" sharedY

          f2 <-
              figure
                      [ ("title"           , bokehExpr "COS Figure")
                      , ("toolbar_location", bokehExpr "right")
                      , ("sizing_mode"     , bokehExpr "stretch_both")
                      ]
                  $ do
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
                            "Legend"
                            [ (["location"], bokehExpr "top_left")
                            , (["click_policy"], bokehExpr "hide")
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
      void $ openWindow "Win2" $ do
          -- each window must have its own data source
          ds <- putDataSource [("x", x), ("sin", ySin), ("cos", yCos)]
          -- each window can have one or more figures
          f1 <-
              figure
                      [ ("title"           , bokehExpr "SIN Figure")
                      , ("toolbar_location", bokehExpr "left")
                      , ("sizing_mode"     , bokehExpr "stretch_both")
                      ]
                  $ do
                        addGlyph
                            "line"
                            ds
                            [ ("x"     , dataField "x")
                            , ("y"     , dataField "sin")
                            , ("color" , dataValue "#1122cc")
                            , ("alpha" , dataValue 0.7)
                            , ("legend", dataValue "SIN Curve")
                            ]
                        linkFigAxis "x_range" sharedX
                        linkFigAxis "y_range" sharedY

          f2 <-
              figure
                      [ ("title"           , bokehExpr "COS Figure")
                      , ("toolbar_location", bokehExpr "right")
                      , ("sizing_mode"     , bokehExpr "stretch_both")
                      ]
                  $ do
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
                            "Legend"
                            [ (["location"], bokehExpr "top_left")
                            , (["click_policy"], bokehExpr "hide")
                            , (["background_fill_alpha"], bokehExpr 0.6)
                            ]
                        linkFigAxis "x_range" sharedX
                        linkFigAxis "y_range" sharedY

          showPlot
              "gridplot"
              [[f1, f2]]
              [ ("merge_tools", bokehExpr False)
              , ("sizing_mode", bokehExpr "stretch_both")
              ]
              jsNull -- null targets the plot to html body element
in
  -- this is what normally entered in web UI, pipe a computation to
  -- a plot specification, thus get a plot definition (with data in it),
  -- this definition passed to 'uiPlot', to trigger the web browser for
  -- visualization.
  doPlot <$> prepareData >>= uiPlot "demoGroup1"

`;

export class HaduiWSC extends HazeWSC {
  // implement ws methods here
}
