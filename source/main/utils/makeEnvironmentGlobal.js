/**
 * Make the main process environment accessible to renderers
*/
export const makeEnvironmentGlobal = (env) => {
  Object.assign(global, { env });
};
