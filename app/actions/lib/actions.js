const actions = {};

export default (definitions, validate) => {
  const newActions = {};
  Object.keys(definitions).forEach((actionName) => {
    const action = newActions[actionName] = (params) => {
      const schema = definitions[actionName];
      validate(schema, params, actionName);
      action.notify(params);
    };
    action.listeners = [];
    action.listen = listener => action.listeners.push(listener);
    action.notify = params => action.listeners.forEach(listener => listener(params));
    action.remove = (listener) => action.listeners.splice(action.listeners.indexOf(listener), 1);
    action.removeAll = () => { action.listeners = []; };
  });
  Object.assign(actions, newActions);
  return newActions;
};

export const resetAllActions = () => {
  Object.keys(actions).forEach((actionName) => {
    actions[actionName].removeAll();
  });
};
