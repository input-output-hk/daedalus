// @flow
export const toJS = (object: ?any): string =>
  typeof object === 'object' ? JSON.parse(JSON.stringify(object)) : object;
