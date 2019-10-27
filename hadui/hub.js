/**
 * Simple broadcasting hub.
 *
 * A plain hub relay all messages verbatimly, from any port connected to it,
 * to all other ports connected.
 *
 * Connecting a port to a plain hub will overwrite the port's `onmessage`,
 * which effectively disconnect it from other receivers connected this way.
 */
export class PlainHub {
  constructor(ports) {
    this.ports = ports ? Array.from(ports) : [];
    this.relayMessage = me => {
      for (let port of this.ports) {
        if (port !== me.target) {
          port.postMessage(me.data);
        }
      }
    };
    for (let port of this.ports) {
      port.onmessage = this.relayMessage;
    }
  }
  addPort(port) {
    this.ports.push(port);
    port.onmessage = this.relayMessage;
  }
}

export default PlainHub;
