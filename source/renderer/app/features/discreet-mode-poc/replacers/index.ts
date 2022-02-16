export type ReplacerFn = (
  isDiscreetMode: boolean,
  symbol: unknown,
  value: unknown
) => unknown;
type ReplacerProps = Record<string, unknown>;
export type DiscreetValueReplacer = (props: ReplacerProps) => ReplacerFn;
