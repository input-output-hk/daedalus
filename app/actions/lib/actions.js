import PropTypes from 'prop-types';

const actions = [];

export default (definitions) => {
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
    actions.push(action);
  });
  return newActions;
};

export const resetAllActions = () => actions.forEach(action => action.removeAll());
