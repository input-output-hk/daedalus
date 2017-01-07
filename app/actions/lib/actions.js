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
  });
  return newActions;
};
