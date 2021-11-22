// @flow

export type DiscreetValueReplacer = () => (
  isDiscreetMode: boolean,
  symbol: string,
  value: any
) => string;
