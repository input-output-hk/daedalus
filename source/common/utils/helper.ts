// @flow
export const toJS = (object: ?any): any =>
  typeof object === 'object' ? JSON.parse(JSON.stringify(object)) : object;
