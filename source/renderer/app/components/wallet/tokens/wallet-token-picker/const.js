// @flow
import type { ScrollPosition, FilterSelectOptions } from './types';

export const MAX_TOKENS = 30;

export const ScrollPositionEnum: EnumMap<string, ScrollPosition> = {
  TOP: 'top',
  MIDDLE: 'middle',
  BOTTOM: 'bottom',
};

export const FilterSelectOptionsEnum: EnumMap<string, FilterSelectOptions> = {
  ALL: 'all',
  FAVORITES: 'favorites',
};
