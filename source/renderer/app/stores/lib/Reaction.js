'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
class Reaction {
  reaction;
  hasBeenStarted;
  dispose;
  constructor(reaction) {
    this.reaction = reaction;
    this.hasBeenStarted = false;
  }
  start() {
    this.dispose = (0, mobx_1.autorun)(() => this.reaction());
    this.hasBeenStarted = true;
  }
  stop() {
    if (this.hasBeenStarted) this.dispose();
  }
}
exports.default = Reaction;
//# sourceMappingURL=Reaction.js.map
