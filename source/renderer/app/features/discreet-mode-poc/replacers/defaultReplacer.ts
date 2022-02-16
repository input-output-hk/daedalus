import type { ReplacerFn } from './index';

export type DefaultReplacer = () => ReplacerFn;
export const defaultReplacer: DefaultReplacer = () => {
  return (isDiscreetMode, symbol, value) => {
    return isDiscreetMode ? symbol : value;
  };
};
