// @flow
export function defaultReplacer() {
  return (isDiscreetMode, symbol: string, value: any) => {
    return isDiscreetMode ? symbol : value;
  };
}
