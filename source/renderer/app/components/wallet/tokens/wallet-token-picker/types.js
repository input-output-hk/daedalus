// @flow
import type { AssetToken } from '../../../../api/assets/types';

export type ScrollPosition = 'top' | 'middle' | 'bottom';

export type FilterSelectOptions = 'all' | 'favorites';

export type Assets = Array<AssetToken>;

export type BooleanMap = { [key: string]: boolean };

export type FilterAssets = {
  assets: Assets,
  filter: FilterSelectOptions,
  tokenFavorites: BooleanMap,
};

export type UseCheckboxes = {
  assets: Assets,
  previousCheckedIds: Array<string>,
};

export type UseFilters = {
  assets: Assets,
  tokenFavorites: BooleanMap,
};

export type GetMaxTokensIdsMap = {
  assetIds: Array<string>,
  previousCheckedIds: Array<string>,
};
