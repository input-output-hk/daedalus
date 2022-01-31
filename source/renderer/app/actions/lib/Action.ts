import { bindAll } from 'lodash';

/**
 * Listener type as Function that takes specific params <P>
 */
export type Listener<P> = (params: P) => any;
/**
 * Action class with typed params
 */

export default class Action<Params> {
  /**
   * Array of all defined actions in the system
   * @type {[Action]}
   */
  static actions: Action<any>[] = [];

  static resetAllActions() {
    Action.actions.forEach((action) => action.removeAll());
  }

  listeners: Listener<Params>[] = [];

  constructor() {
    bindAll(this, ['trigger']);
    Action.actions.push(this);
  }

  listen(listener: Listener<Params>) {
    this.listeners.push(listener);
  }

  trigger(params: Params) {
    this.listeners.forEach((listener) => listener(params));
  }

  remove(listener: Listener<Params>) {
    this.listeners.splice(this.listeners.indexOf(listener), 1);
  }

  removeAll() {
    this.listeners = [];
  }

  once(listener: Listener<Params>) {
    this.listeners.push((...args) => {
      this.remove(listener);
      listener(...args);
    });
  }
}
