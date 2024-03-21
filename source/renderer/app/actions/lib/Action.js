'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const lodash_1 = require('lodash');
/**
 * Action class with typed params
 */
class Action {
  /**
   * Array of all defined actions in the system
   * @type {[Action]}
   */
  static actions = [];
  static resetAllActions() {
    Action.actions.forEach((action) => action.removeAll());
  }
  listeners = [];
  constructor() {
    (0, lodash_1.bindAll)(this, ['trigger']);
    Action.actions.push(this);
  }
  listen(listener) {
    this.listeners.push(listener);
  }
  async trigger(params) {
    await Promise.all(this.listeners.map((listener) => listener(params)));
  }
  remove(listener) {
    this.listeners.splice(this.listeners.indexOf(listener), 1);
  }
  removeAll() {
    this.listeners = [];
  }
  once(listener) {
    this.listeners.push((...args) => {
      this.remove(listener);
      listener(...args);
    });
  }
}
exports.default = Action;
//# sourceMappingURL=Action.js.map
