export type ReplacerFn = (
  isDiscreetMode: boolean,
  symbol: string,
  value: any
) => string;
type ReplacerProps = Record<string, any>;
export type DiscreetValueReplacer = (props: ReplacerProps) => ReplacerFn;
