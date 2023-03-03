export const toJS = (object: any | null | undefined): any =>
  typeof object === 'object' ? JSON.parse(JSON.stringify(object)) : object;
