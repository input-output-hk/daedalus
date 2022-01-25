// @flow
import type { Intl } from '../../../../types/i18nTypes';
import type { AssetToken } from '../../../../api/assets/types';

export type Props = {
  intl: Intl,
  assets: Array<AssetToken>,
  walletName: string,
  tokenFavorites: Object,
  previouslyCheckedIds?: Array<string>,
  onAdd: Function,
  onCancel: Function,
};

export type ItemProps = {
  intl: Intl,
  className: string,
  isChecked: boolean,
  isMaxCount: boolean,
  isPreviouslyChecked: boolean,
  uniqueId: string,
  toggleCheckbox: Function,
};

export type ScrollPosition = 'top' | 'middle' | 'bottom';

export type FilterSelectOptions = 'all' | 'favorites';

export type ToggleAllMode =
  | 'clearAll'
  | 'clearAllDisabled'
  | 'selectAll'
  | 'selectAllDisabled';

export type Assets = Array<AssetToken>;

export type BooleanMap = { [key: string]: boolean };

export type FilterAssets = {
  assets: Assets,
  filter: FilterSelectOptions,
  tokenFavorites: BooleanMap,
};

export type UseCheckboxes = {
  assets: Assets,
  currentAssets: Assets,
  previouslyCheckedIds: Array<string>,
};

export type UseFilters = {
  assets: Assets,
  tokenFavorites: BooleanMap,
};

export type GetMaxTokensIdMap = {
  checkedIds: Array<string>,
  currentAssetIds: Array<string>,
  previouslyCheckedIds: Array<string>,
};

export type GetTokenCounterText = {
  assets: Assets,
  currentAssets: Assets,
};

export type ClearSelection = {
  checkboxes: BooleanMap,
  currentAssetIds: Array<string>,
};

export type GetToggleAllMode = {
  isMaxTotalCount: boolean,
  isMaxCurrentCount: boolean,
  currentCheckedCount: number,
};

export type GetEnabledAssetIds = {
  assets: Assets,
  previouslyCheckedIdsSet: Set<string>,
};

export type GetCurrentCheckedIds = ClearSelection;
