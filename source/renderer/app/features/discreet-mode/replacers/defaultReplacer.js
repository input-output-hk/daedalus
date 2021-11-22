// @flow

import type { DiscreetValueReplacer } from '../types';

export const defaultReplacer: DiscreetValueReplacer = () => {
  return (isDiscreetMode, symbol, value) => {
    return isDiscreetMode ? symbol : value;
  };
};
