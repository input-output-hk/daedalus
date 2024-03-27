'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createReactions = exports.Reaction = void 0;
const mobx_1 = require('mobx');
class Reaction {
  reaction;
  isRunning = false;
  dispose = null;
  constructor(reaction) {
    this.reaction = reaction;
  }
  start() {
    if (!this.isRunning) {
      this.dispose = (0, mobx_1.autorun)(this.reaction);
      this.isRunning = true;
    }
  }
  stop() {
    if (this.isRunning && this.dispose) {
      this.dispose();
      this.isRunning = false;
    }
  }
}
exports.Reaction = Reaction;
const createReactions = (reactions) => reactions.map((r) => new Reaction(r));
exports.createReactions = createReactions;
//# sourceMappingURL=reaction.js.map
