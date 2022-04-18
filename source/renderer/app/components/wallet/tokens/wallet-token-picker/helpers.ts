import { messages } from './WalletTokenPicker.messages';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
  ToggleModeEnum,
} from './const';
import type { Intl } from '../../../../types/i18nTypes';
import type {
  Assets,
  BooleanMap,
  ClearSelection,
  FilterAssets,
  ScrollPosition,
  GetMaxTokensIdMap,
  GetTokenCounterText,
  GetCurrentCheckedIds,
  GetToggleAllMode,
  GetEnabledAssetIds,
} from './types';

const isScrollAtTop = (element: HTMLElement) => element.scrollTop === 0;

const isScrollAtBottom = (element: HTMLElement) =>
  element.scrollHeight - element.scrollTop === element.clientHeight;

export const getScrollPosition = (element: EventTarget): ScrollPosition => {
  if (!(element instanceof HTMLElement)) {
    return ScrollPositionEnum.TOP;
  }

  if (isScrollAtTop(element)) {
    return ScrollPositionEnum.TOP;
  }

  if (isScrollAtBottom(element)) {
    return ScrollPositionEnum.BOTTOM;
  }

  return ScrollPositionEnum.MIDDLE;
};
export const getMaxTokensIdMap = ({
  checkedIds,
  currentAssetIds,
  previouslyCheckedIds,
}: GetMaxTokensIdMap) => {
  return [...new Set([...checkedIds, ...currentAssetIds])]
    .slice(0, MAX_TOKENS - previouslyCheckedIds.length)
    .reduce((acc: BooleanMap, element) => ({ ...acc, [element]: true }), {});
};
export const clearSelection = ({
  checkboxes,
  currentAssetIds,
}: ClearSelection) => {
  return {
    ...checkboxes,
    ...currentAssetIds.reduce(
      (acc, assestId) => ({ ...acc, [assestId]: false }),
      {} as BooleanMap
    ),
  };
};
export const getEnabledAssetIds = ({
  assets,
  previouslyCheckedIdsSet,
}: GetEnabledAssetIds) =>
  assets.reduce(
    (acc, { uniqueId }) =>
      previouslyCheckedIdsSet.has(uniqueId) ? acc : [...acc, uniqueId],
    []
  );
export const getCheckedIds = (checkBoxes: BooleanMap) =>
  Object.entries(checkBoxes).reduce(
    (acc, [id, checked]) => (checked ? [...acc, id] : acc),
    []
  );
export const getCurrentCheckedIds = ({
  checkboxes,
  currentAssetIds,
}: GetCurrentCheckedIds): Array<string> =>
  currentAssetIds.filter((assetId) => checkboxes[assetId]);
export const filterSelectOptions = (intl: Intl) => [
  {
    label: intl.formatMessage(messages.allTokensLabel),
    value: FilterSelectOptionsEnum.ALL,
  },
  {
    label: intl.formatMessage(messages.favoriteTokensLabel),
    value: FilterSelectOptionsEnum.FAVORITES,
  },
];
export const filterAssets = ({
  assets,
  filter,
  tokenFavorites,
}: FilterAssets): Assets => {
  return assets.filter((asset) => {
    return filter === FilterSelectOptionsEnum.FAVORITES
      ? tokenFavorites[asset.uniqueId]
      : true;
  });
};
export const getToggleAllLabel = (isClearAllMode: boolean) =>
  isClearAllMode ? 'clearAll' : 'checkAllLabel';
export const getTokenCounterText = ({
  assets,
  currentAssets,
}: GetTokenCounterText) => {
  if (assets.length === currentAssets.length) {
    return ` (${assets.length})`;
  }

  return ` (${currentAssets.length} / ${assets.length})`;
};

const isAllCurrentDisabled = ({
  isMaxCurrentCount,
  currentCheckedCount,
}: GetToggleAllMode) => currentCheckedCount === 0 && isMaxCurrentCount;

const isMaxedAndCurrentCleared = ({
  isMaxTotalCount,
  currentCheckedCount,
}: GetToggleAllMode) => isMaxTotalCount && currentCheckedCount === 0;

const isMaxedAndSomeSelected = ({
  isMaxCurrentCount,
  isMaxTotalCount,
  currentCheckedCount,
}: GetToggleAllMode) =>
  (isMaxTotalCount || isMaxCurrentCount) && currentCheckedCount;

export const getToggleAllMode = (args: GetToggleAllMode) => {
  if (isMaxedAndSomeSelected(args)) {
    return ToggleModeEnum.CLEAR_ALL;
  }

  if (isAllCurrentDisabled(args)) {
    return ToggleModeEnum.CLEAR_ALL_DISABLED;
  }

  if (isMaxedAndCurrentCleared(args)) {
    return ToggleModeEnum.SELECT_ALL_DISABLED;
  }

  return ToggleModeEnum.SELECT_ALL;
};
