/**
 * hadui-custom.js
 */

import { HazeWSC } from "/haze.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `do
  print "Bokeh rocks!"

  showHazePage "bokeh-poc"
`;

export class HaduiWSC extends HazeWSC {
  // implement ws methods here
}
