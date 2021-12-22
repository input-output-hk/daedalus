// @flow
import type { Intl } from '../../../../types/i18nTypes';
import type { AssetToken } from '../../../../api/assets/types';

export type Props = {
  intl: Intl,
  assets: Array<AssetToken>,
  tokenFavorites: Object,
  previousCheckedIds?: Array<string>,
  onAdd: Function,
  onCancel: Function,
};

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
