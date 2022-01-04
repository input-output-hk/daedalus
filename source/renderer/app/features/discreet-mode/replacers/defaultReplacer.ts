import type { ReplacerFn } from '../types';

export type DefaultReplacer = () => ReplacerFn;
export const defaultReplacer: DefaultReplacer =
  () => (isDiscreetMode, symbol, value) =>
    isDiscreetMode ? symbol : value;
