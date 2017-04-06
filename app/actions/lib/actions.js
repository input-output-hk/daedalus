// @flow
import PropTypes from 'prop-types';
import { bindAll } from 'lodash';

/**
 * Listener type as Function that takes specific params <P>
 */
export type Listener<P> = (params: P) => any;

/**
 * Action class with typed params
 */
export class Action<Params> {

  /**
   * Array of all defined actions in the system
   * @type {[Action]}
   */
  static actions: Action<any>[] = [];

  static resetAllActions() {
    Action.actions.forEach(action => action.removeAll());
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
    this.listeners.forEach(listener => listener(params));
  }

  remove(listener: Listener<Params>) {
    this.listeners.splice(self.listeners.indexOf(listener), 1);
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

/**
 * Typed object with keys that map to Actions
 */
export type ActionMap = { [key: string]: Action<*> };

// ======= OLD ACTIONS ======

const actions = [];

export default (definitions: Object) => {
  const newActions = {};
  Object.keys(definitions).forEach((actionName) => {
    const action = newActions[actionName] = (params) => {
      const schema = definitions[actionName];
      PropTypes.validateWithErrors(schema, params, actionName);
      action.notify(params);
    };
    action.listeners = [];
    action.listen = listener => action.listeners.push(listener);
    action.notify = params => action.listeners.forEach(listener => listener(params));
    action.remove = (listener) => action.listeners.splice(action.listeners.indexOf(listener), 1);
    action.removeAll = () => { action.listeners = []; };
    action.once = listener => action.listeners.push((...args) => {
      action.remove(listener);
      listener(...args);
    });
    actions.push(action);
  });
  return newActions;
};

export const resetAllActions = () => actions.forEach(action => action.removeAll());
