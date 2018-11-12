// @flow
export const getOsPlatform = (isNodeEnvironment: boolean): string => {
  if (isNodeEnvironment) {
    const nodeRequire = require;
    return nodeRequire('os').platform();
  }
  return global.os.platform;
};
