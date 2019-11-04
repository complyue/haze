/**
 * haze.js
 */

import WSC from "/wsc.js";

const plotData = {};

// reply plot data to a haze window after it reported-in
window.onmessage = function(me) {
  let hazeWin = me.source;

  if ("haze-win-report-in" !== me.data.type) {
    console.error("unexpected window message.");
    debugger;
    return;
  }

  let pd = plotData[hazeWin.name];
  delete plotData[hazeWin.name];
  hazeWin.postMessage(
    Object.assign(
      {
        type: "haze-plot-data"
      },
      pd
    ),
    "*",
    pd.bins // transfer the typed-arrays for zero-copy performance
  );
};

export class HazeWSC extends WSC {
  // ws methods for plotting here

  async plotWin(pgid, pwid, cnl, plotCode) {
    let bins = this.bins;
    this.bins = [];

    let winName = "plot@" + pgid + "%" + pwid;
    plotData[winName] = {
      bins,
      pgid,
      pwid,
      cnl,
      plotCode
    };
    window.open("/haze.html", winName);
  }
}
