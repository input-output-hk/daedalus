// @flow
export const ellipsis = (
  str: string,
  minCharsInit: number,
  minCharsEnd?: ?number
) => {
  if (str.length <= minCharsInit) return str;

  const initStr = str.substr(0, minCharsInit);
  const shouldHaveEndStr = () =>
    minCharsEnd && str.length - minCharsInit > minCharsEnd;
  const endStr =
    minCharsEnd && shouldHaveEndStr()
      ? str.substr(str.length - minCharsEnd)
      : '';
  return `${initStr}\u2026${endStr}`;
};
