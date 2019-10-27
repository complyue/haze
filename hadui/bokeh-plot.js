/**
 * bokeh-plot.js
 */

opener.onmessage = function(me) {
  if (opener !== me.source) {
    console.error("window msg from other than opener ?!", me.source);
    debugger;
  }
  // process ctrl msg here
};

const commCh = new MessageChannel();

commCh.port1.onmessage = function(me) {
  // process comm msg here
};

opener.postMessage({ type: "plot-win-report-in" }, "*", [commCh.port2]);
