// @flow

export type ReplacerFn = (
  isDiscreetMode: boolean,
  symbol: string,
  value: any
) => string;

export type DiscreetValueReplacer = (args: any) => ReplacerFn;
