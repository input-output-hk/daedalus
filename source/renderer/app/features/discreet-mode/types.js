// @flow

export type ReplacerFn = (
  isDiscreetMode: boolean,
  symbol: string,
  value: any
) => string;

type ReplacerProps = {
  [string]: any,
};

export type DiscreetValueReplacer = (props: ReplacerProps) => ReplacerFn;
