// @flow

export const ellipsis = (str:string, maxChars:number) => (
  str.length > maxChars ? str.substr(0, maxChars) + '\u2026' : str
);

export const objectToHumanReadableString = (obj:{}) =>
  Object.entries(obj)
    .map(([key, value]) => `${key}: ${String(value)}`)
    .join(', ');
