export const removeCharAtPosition = (str: string, pos: number) =>
  str.substring(0, pos) + str.substring(pos + 1, str.length);
