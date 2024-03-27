'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getToggleAllMode = exports.getTokenCounterText = exports.getToggleAllLabel = exports.filterAssets = exports.filterSelectOptions = exports.getCurrentCheckedIds = exports.getCheckedIds = exports.getEnabledAssetIds = exports.clearSelection = exports.getMaxTokensIdMap = exports.getScrollPosition = void 0;
const WalletTokenPicker_messages_1 = require('./WalletTokenPicker.messages');
const const_1 = require('./const');
const isScrollAtTop = (element) => element.scrollTop === 0;
const isScrollAtBottom = (element) =>
  element.scrollHeight - element.scrollTop === element.clientHeight;
const getScrollPosition = (element) => {
  if (!(element instanceof HTMLElement)) {
    return const_1.ScrollPositionEnum.TOP;
  }
  if (isScrollAtTop(element)) {
    return const_1.ScrollPositionEnum.TOP;
  }
  if (isScrollAtBottom(element)) {
    return const_1.ScrollPositionEnum.BOTTOM;
  }
  return const_1.ScrollPositionEnum.MIDDLE;
};
exports.getScrollPosition = getScrollPosition;
const getMaxTokensIdMap = ({
  checkedIds,
  currentAssetIds,
  previouslyCheckedIds,
}) => {
  return [...new Set([...checkedIds, ...currentAssetIds])]
    .slice(0, const_1.MAX_TOKENS - previouslyCheckedIds.length)
    .reduce((acc, element) => ({ ...acc, [element]: true }), {});
};
exports.getMaxTokensIdMap = getMaxTokensIdMap;
const clearSelection = ({ checkboxes, currentAssetIds }) => {
  return {
    ...checkboxes,
    ...currentAssetIds.reduce(
      (acc, assestId) => ({ ...acc, [assestId]: false }),
      {}
    ),
  };
};
exports.clearSelection = clearSelection;
const getEnabledAssetIds = ({ assets, previouslyCheckedIdsSet }) =>
  assets.reduce(
    (acc, { uniqueId }) =>
      previouslyCheckedIdsSet.has(uniqueId) ? acc : [...acc, uniqueId],
    []
  );
exports.getEnabledAssetIds = getEnabledAssetIds;
const getCheckedIds = (checkBoxes) =>
  Object.entries(checkBoxes).reduce(
    (acc, [id, checked]) => (checked ? [...acc, id] : acc),
    []
  );
exports.getCheckedIds = getCheckedIds;
const getCurrentCheckedIds = ({ checkboxes, currentAssetIds }) =>
  currentAssetIds.filter((assetId) => checkboxes[assetId]);
exports.getCurrentCheckedIds = getCurrentCheckedIds;
const filterSelectOptions = (intl) => [
  {
    label: intl.formatMessage(
      WalletTokenPicker_messages_1.messages.allTokensLabel
    ),
    value: const_1.FilterSelectOptionsEnum.ALL,
  },
  {
    label: intl.formatMessage(
      WalletTokenPicker_messages_1.messages.favoriteTokensLabel
    ),
    value: const_1.FilterSelectOptionsEnum.FAVORITES,
  },
];
exports.filterSelectOptions = filterSelectOptions;
const filterAssets = ({ assets, filter, tokenFavorites }) => {
  return assets.filter((asset) => {
    return filter === const_1.FilterSelectOptionsEnum.FAVORITES
      ? tokenFavorites[asset.uniqueId]
      : true;
  });
};
exports.filterAssets = filterAssets;
const getToggleAllLabel = (isClearAllMode) =>
  isClearAllMode ? 'clearAll' : 'checkAllLabel';
exports.getToggleAllLabel = getToggleAllLabel;
const getTokenCounterText = ({ assets, currentAssets }) => {
  if (assets.length === currentAssets.length) {
    return ` (${assets.length})`;
  }
  return ` (${currentAssets.length} / ${assets.length})`;
};
exports.getTokenCounterText = getTokenCounterText;
const isAllCurrentDisabled = ({ isMaxCurrentCount, currentCheckedCount }) =>
  currentCheckedCount === 0 && isMaxCurrentCount;
const isMaxedAndCurrentCleared = ({ isMaxTotalCount, currentCheckedCount }) =>
  isMaxTotalCount && currentCheckedCount === 0;
const isMaxedAndSomeSelected = ({
  isMaxCurrentCount,
  isMaxTotalCount,
  currentCheckedCount,
}) => (isMaxTotalCount || isMaxCurrentCount) && currentCheckedCount;
const getToggleAllMode = (args) => {
  if (isMaxedAndSomeSelected(args)) {
    return const_1.ToggleModeEnum.CLEAR_ALL;
  }
  if (isAllCurrentDisabled(args)) {
    return const_1.ToggleModeEnum.CLEAR_ALL_DISABLED;
  }
  if (isMaxedAndCurrentCleared(args)) {
    return const_1.ToggleModeEnum.SELECT_ALL_DISABLED;
  }
  return const_1.ToggleModeEnum.SELECT_ALL;
};
exports.getToggleAllMode = getToggleAllMode;
//# sourceMappingURL=helpers.js.map
