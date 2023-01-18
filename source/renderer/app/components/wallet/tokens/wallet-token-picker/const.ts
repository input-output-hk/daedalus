import type {
  ScrollPosition,
  FilterSelectOptions,
  ToggleAllMode,
} from './types';

export const MAX_TOKENS = 30;
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const ScrollPositionEnum: EnumMap<string, ScrollPosition> = {
  TOP: 'top',
  MIDDLE: 'middle',
  BOTTOM: 'bottom',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const FilterSelectOptionsEnum: EnumMap<string, FilterSelectOptions> = {
  ALL: 'all',
  FAVORITES: 'favorites',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const ToggleModeEnum: EnumMap<string, ToggleAllMode> = {
  CLEAR_ALL: 'clearAll',
  CLEAR_ALL_DISABLED: 'clearAllDisabled',
  SELECT_ALL: 'selectAll',
  SELECT_ALL_DISABLED: 'selectAllDisabled',
};
