/**
 * hadui-custom.js
 */

import { BokehWSC } from "/bokeh-ctrl.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `do
  print "Bokeh rocks!"

  showBokehPage "bokeh-poc"
`;

export class HaduiWSC extends BokehWSC {
  // implement ws methods here
}
