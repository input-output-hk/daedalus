// @flow
import type { AssetToken } from '../../../../api/assets/types';

export type ScrollPosition = 'top' | 'middle' | 'bottom';

export type FilterSelectOptions = 'all' | 'favorites';

export type CheckBoxes = { [key: string]: boolean };

export type Assets = Array<AssetToken>;

export type FilterAssets = {
  assets: Assets,
  filter: FilterSelectOptions,
  tokenFavorites: { [key: string]: boolean },
};

export type UseCheckboxes = {
  assets: Assets,
};

export type UseFilters = {
  assets: Assets,
  tokenFavorites: { [key: string]: boolean },
};
