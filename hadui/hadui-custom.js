/**
 * hadui-custom.js
 */

import WSC from "/wsc.js";

export const HaduiDefaultStmt =
  // the statement shown initially
  `do
  print "Bokeh rocks!"

  showBokehPage "bokeh-poc"
`;

export class HaduiWSC extends WSC {
  // implement ws methods here
}
