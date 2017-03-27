// @flow

export const eclipseString = (str:string, maxChars:number) => (
  str.length > maxChars ? str.substr(0, maxChars) + '\u2026' : str
);
