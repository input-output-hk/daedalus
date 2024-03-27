'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.Feature = void 0;
const pull_1 = __importDefault(require('lodash/pull'));
class Feature {
  isRunning = false;
  reactions = [];
  async start() {
    this.startReactions();
    this.isRunning = true;
  }
  async stop() {
    this.stopReactions();
    this.isRunning = false;
  }
  // REACTIONS
  registerReactions(reactions) {
    this.reactions.push(...reactions);
  }
  unregisterReactions(reactions) {
    (0, pull_1.default)(this.reactions, ...reactions);
  }
  startReactions(reactions = this.reactions) {
    reactions.forEach((r) => r.start());
  }
  stopReactions(reactions = this.reactions) {
    reactions.forEach((r) => r.stop());
  }
}
exports.Feature = Feature;
//# sourceMappingURL=feature.js.map
