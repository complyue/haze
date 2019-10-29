/**
 * Synchronize a set of named ranges over the broadcast channel.
 */
export default class RangeSync {
  constructor(ch, ranges, debug = false) {
    this.ch = ch;
    this.keys = [];
    this.ranges = [];
    let cntr = 0;
    for (const [key, rng] of ranges.entries
      ? ranges.entries()
      : Object.entries(ranges)) {
      const i = cntr;
      this.keys.push(key);
      this.ranges.push(rng);
      rng.callback = () => {
        if (!this.dirty) {
          // triggered right here to `callback` assignment, ignore
          return;
        }
        this.dirty[i] = true;
        this.scheduleAnnouncement();
      };
      cntr++;
    }
    this.dirty = new Array(cntr);
    this.synced = new Array(cntr);
    this.debug = !!debug;

    ch.onmessage = me => {
      for (const [key, upd] of me.data) {
        for (let i = 0; i < this.keys.length; i++) {
          if (key === this.keys[i]) {
            const sr = this.synced[i];
            if (!sr || sr[0] !== upd[0] || sr[1] !== upd[1]) {
              if (this.debug) {
                console.debug("acking:", key, upd, this.synced[i]);
              }
              this.synced[i] = upd;
              this.ranges[i].setv({
                start: upd[0],
                end: upd[1]
              });
            }
          }
        }
      }
    };
    this.announceTimer = null;
  }

  scheduleAnnouncement() {
    if (null !== this.announceTimer) {
      return; // already scheduled
    }
    this.announceTimer = setTimeout(() => {
      this.announceTimer = null; // clear for next schedule
      const upds = [];
      for (let i = 0; i < this.keys.length; i++) {
        if (this.dirty[i]) {
          this.dirty[i] = false;
          const rng = this.ranges[i];
          const sr = this.synced[i];
          if (sr && sr[0] === rng.start && sr[1] === rng.end) {
            // well sync'ed, no need to announce
          } else {
            // really need to announce
            const key = this.keys[i];
            const rd = [rng.start, rng.end];
            if (this.debug) {
              console.debug("announcing:", key, rd, sr);
            }
            this.synced[i] = rd;
            upds.push([key, rd]);
          }
        }
      }
      if (upds.length > 0) {
        this.ch.postMessage(upds);
      }
    }, 1000); // announce at most 1Hz
  }
}
