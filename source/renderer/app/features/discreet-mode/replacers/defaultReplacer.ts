import type { ReplacerFn } from '../types';

export type DefaultReplacer = () => ReplacerFn;
export const defaultReplacer: DefaultReplacer = () => {
  return (isDiscreetMode, symbol, value) => {
    return isDiscreetMode ? symbol : value;
  };
};
