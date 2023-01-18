import { useState, useEffect, useCallback, useMemo } from 'react';
import { debounce } from 'lodash';
import { searchAssets } from '../../../../utils/assets';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
  ToggleModeEnum,
} from './const';
import {
  getEnabledAssetIds,
  getCheckedIds,
  filterAssets,
  getScrollPosition,
  getMaxTokensIdMap,
  getCurrentCheckedIds,
  clearSelection,
  getToggleAllMode,
} from './helpers';
import type {
  Assets,
  FilterSelectOptions,
  ScrollPosition,
  UseCheckboxes,
  UseFilters,
  BooleanMap,
} from './types';

export const useCheckboxes = ({
  assets,
  currentAssets,
  previouslyCheckedIds,
}: UseCheckboxes) => {
  const [checkboxes, setCheckboxes] = useState<BooleanMap>({});
  const checkedIds = useMemo(() => getCheckedIds(checkboxes), [checkboxes]);
  const previouslyCheckedIdsSet = useMemo(
    () => new Set<string>(previouslyCheckedIds),
    [previouslyCheckedIds]
  );
  const currentAssetIds = useMemo(
    () =>
      getEnabledAssetIds({
        assets: currentAssets,
        previouslyCheckedIdsSet,
      }),
    [currentAssets, previouslyCheckedIdsSet]
  );
  const currentCheckedIds = useMemo(
    () =>
      getCurrentCheckedIds({
        checkboxes,
        currentAssetIds,
      }),
    [checkboxes, currentAssetIds]
  );
  const currentCheckedCount = currentCheckedIds.length;
  const totalCheckedCount = checkedIds.length + previouslyCheckedIdsSet.size;
  const isMaxTotalCount =
    totalCheckedCount === Math.min(MAX_TOKENS, assets.length);
  const isMaxCurrentCount =
    currentCheckedCount === Math.min(MAX_TOKENS, currentAssetIds.length);
  const clearAll = useCallback(
    () =>
      setCheckboxes(
        clearSelection({
          checkboxes,
          currentAssetIds,
        })
      ),
    [setCheckboxes, checkboxes, currentAssetIds]
  );
  const checkMax = useCallback(() => {
    setCheckboxes(
      getMaxTokensIdMap({
        checkedIds,
        currentAssetIds,
        previouslyCheckedIds,
      })
    );
  }, [currentAssetIds, setCheckboxes, previouslyCheckedIds, checkedIds]);
  const toggleAllMode = getToggleAllMode({
    isMaxCurrentCount,
    isMaxTotalCount,
    currentCheckedCount,
  });
  const isClearAllMode = [
    ToggleModeEnum.CLEAR_ALL,
    ToggleModeEnum.CLEAR_ALL_DISABLED,
  ].includes(toggleAllMode);
  const isToggleAllDisabled = [
    ToggleModeEnum.SELECT_ALL_DISABLED,
    ToggleModeEnum.CLEAR_ALL_DISABLED,
  ].includes(toggleAllMode);
  const toggleAllFn = isClearAllMode ? clearAll : checkMax;
  const toggleCheckbox = useCallback(
    (assetId: string) => {
      const newValue = !checkboxes[assetId];

      if (totalCheckedCount < MAX_TOKENS || !newValue) {
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
export const useFilters = ({ assets, tokenFavorites }: UseFilters) => {
  const [searchValue, setSearchValue] = useState<string>('');
  const [currentAssets, setCurrentAssets] = useState<Assets>(assets);
  const [filterOption, setFilterOption] = useState<FilterSelectOptions>(
    FilterSelectOptionsEnum.ALL
  );
  useEffect(() => {
    const searchedAssets = searchAssets(searchValue, assets);
    const filterdAssets = filterAssets({
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
export const useScrollPosition = () => {
  const [scrollPosition, setScrollPosition] = useState<ScrollPosition>(
    ScrollPositionEnum.TOP
  );

  const setPosition = (target: EventTarget) =>
    setScrollPosition(getScrollPosition(target));

  const debouncedSetPosition = useCallback(
    debounce(setPosition, 100, {
      leading: true,
    }),
    [setPosition]
  );

  const onScroll = (evt: React.MouseEvent<HTMLElement>) => {
    evt.persist();
    debouncedSetPosition(evt.target);
  };

  return {
    scrollPosition,
    onScroll,
  };
};
