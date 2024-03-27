'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Reaction_1 = __importDefault(require('./Reaction'));
class Store {
  api;
  actions;
  analytics;
  stores;
  environment = global.environment;
  _reactions = [];
  constructor(
    api,
    actions,
    analytics // eslint-disable-next-line no-empty-function
  ) {
    this.api = api;
    this.actions = actions;
    this.analytics = analytics;
  }
  registerReactions(reactions) {
    reactions.forEach((reaction) =>
      this._reactions.push(new Reaction_1.default(reaction))
    );
  }
  configure(stores) {
    this.stores = stores;
  }
  setup() {}
  initialize() {
    this.setup();
    this._reactions.forEach((reaction) => reaction.start());
  }
  teardown() {
    this._reactions.forEach((reaction) => reaction.stop());
  }
}
exports.default = Store;
//# sourceMappingURL=Store.js.map
