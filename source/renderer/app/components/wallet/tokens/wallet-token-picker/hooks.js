'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.useScrollPosition = exports.useFilters = exports.useCheckboxes = void 0;
const react_1 = require('react');
const lodash_1 = require('lodash');
const assets_1 = require('../../../../utils/assets');
const const_1 = require('./const');
const helpers_1 = require('./helpers');
const useCheckboxes = ({ assets, currentAssets, previouslyCheckedIds }) => {
  const [checkboxes, setCheckboxes] = (0, react_1.useState)({});
  const checkedIds = (0, react_1.useMemo)(
    () => (0, helpers_1.getCheckedIds)(checkboxes),
    [checkboxes]
  );
  const previouslyCheckedIdsSet = (0, react_1.useMemo)(
    () => new Set(previouslyCheckedIds),
    [previouslyCheckedIds]
  );
  const currentAssetIds = (0, react_1.useMemo)(
    () =>
      (0, helpers_1.getEnabledAssetIds)({
        assets: currentAssets,
        previouslyCheckedIdsSet,
      }),
    [currentAssets, previouslyCheckedIdsSet]
  );
  const currentCheckedIds = (0, react_1.useMemo)(
    () =>
      (0, helpers_1.getCurrentCheckedIds)({
        checkboxes,
        currentAssetIds,
      }),
    [checkboxes, currentAssetIds]
  );
  const currentCheckedCount = currentCheckedIds.length;
  const totalCheckedCount = checkedIds.length + previouslyCheckedIdsSet.size;
  const isMaxTotalCount =
    totalCheckedCount === Math.min(const_1.MAX_TOKENS, assets.length);
  const isMaxCurrentCount =
    currentCheckedCount ===
    Math.min(const_1.MAX_TOKENS, currentAssetIds.length);
  const clearAll = (0, react_1.useCallback)(
    () =>
      setCheckboxes(
        (0, helpers_1.clearSelection)({
          checkboxes,
          currentAssetIds,
        })
      ),
    [setCheckboxes, checkboxes, currentAssetIds]
  );
  const checkMax = (0, react_1.useCallback)(() => {
    setCheckboxes(
      (0, helpers_1.getMaxTokensIdMap)({
        checkedIds,
        currentAssetIds,
        previouslyCheckedIds,
      })
    );
  }, [currentAssetIds, setCheckboxes, previouslyCheckedIds, checkedIds]);
  const toggleAllMode = (0, helpers_1.getToggleAllMode)({
    isMaxCurrentCount,
    isMaxTotalCount,
    currentCheckedCount,
  });
  const isClearAllMode = [
    const_1.ToggleModeEnum.CLEAR_ALL,
    const_1.ToggleModeEnum.CLEAR_ALL_DISABLED,
  ].includes(toggleAllMode);
  const isToggleAllDisabled = [
    const_1.ToggleModeEnum.SELECT_ALL_DISABLED,
    const_1.ToggleModeEnum.CLEAR_ALL_DISABLED,
  ].includes(toggleAllMode);
  const toggleAllFn = isClearAllMode ? clearAll : checkMax;
  const toggleCheckbox = (0, react_1.useCallback)(
    (assetId) => {
      const newValue = !checkboxes[assetId];
      if (totalCheckedCount < const_1.MAX_TOKENS || !newValue) {
        setCheckboxes({ ...checkboxes, [assetId]: newValue });
      }
    },
    [checkboxes]
  );
  return {
    checkboxes,
    totalCheckedCount,
    checkedIds,
    isMaxTotalCount,
    isToggleAllDisabled,
    previouslyCheckedIdsSet,
    toggleAllFn,
    toggleCheckbox,
    isClearAllMode,
  };
};
exports.useCheckboxes = useCheckboxes;
const useFilters = ({ assets, tokenFavorites }) => {
  const [searchValue, setSearchValue] = (0, react_1.useState)('');
  const [currentAssets, setCurrentAssets] = (0, react_1.useState)(assets);
  const [filterOption, setFilterOption] = (0, react_1.useState)(
    const_1.FilterSelectOptionsEnum.ALL
  );
  (0, react_1.useEffect)(() => {
    const searchedAssets = (0, assets_1.searchAssets)(searchValue, assets);
    const filterdAssets = (0, helpers_1.filterAssets)({
      assets: searchedAssets,
      filter: filterOption,
      tokenFavorites,
    });
    setCurrentAssets(filterdAssets);
  }, [assets, searchValue, filterOption, tokenFavorites]);
  return {
    searchValue,
    setSearchValue,
    currentAssets,
    filterOption,
    setFilterOption,
  };
};
exports.useFilters = useFilters;
const useScrollPosition = () => {
  const [scrollPosition, setScrollPosition] = (0, react_1.useState)(
    const_1.ScrollPositionEnum.TOP
  );
  const setPosition = (target) =>
    setScrollPosition((0, helpers_1.getScrollPosition)(target));
  const debouncedSetPosition = (0, react_1.useCallback)(
    (0, lodash_1.debounce)(setPosition, 100, {
      leading: true,
    }),
    [setPosition]
  );
  const onScroll = (evt) => {
    evt.persist();
    debouncedSetPosition(evt.target);
  };
  return {
    scrollPosition,
    onScroll,
  };
};
exports.useScrollPosition = useScrollPosition;
//# sourceMappingURL=hooks.js.map
