/**
 * haze-plot.js
 */

function syncRange(rng, axisName) {
  let ch = new BroadcastChannel(axisName);
  let announceTimer = null;
  ch.onmessage = function(me) {
    let rd = me.data;
    if (null !== announceTimer) {
      return; // local updates on the way
    }
    if (rd[0] === rng.start && rd[1] === rng.end) {
      return; // well synced, don't propagate
    }
    rng.setv({
      start: rd[0],
      end: rd[1]
    });
  };
  rng.callback = () => {
    if (null !== announceTimer) {
      return; // already scheduled
    }
    announceTimer = setTimeout(() => {
      announceTimer = null; // clear for next schedule
      const rd = [rng.start, rng.end];
      ch.postMessage(rd);
    }, 1000); // announce at most 1Hz
  };
}

window.onmessage = function(me) {
  if (opener !== me.source) {
    console.error("window msg from other than opener ?!", me.source);
    debugger;
    return;
  }
  switch (me.data.type) {
    case "haze-plot-data":
      let { bins, pgid, pwid, cnl, plotCode } = me.data;
      let plotProcedure = eval(plotCode);
      let cdsa = [];
      for (let cols of cnl) {
        let cdd = {};
        for (let col of cols) {
          cdd[col] = new this.Float64Array(bins.pop());
        }
        cdsa.push(new Bokeh.ColumnDataSource({ data: cdd }));
      }
      setTimeout(plotProcedure, 0, pgid, pwid, cdsa);
      return;

    default:
      console.error("unexpected msg type from opener ?!", me.data);
      debugger;
  }
};

opener.postMessage({ type: "haze-win-report-in" });
