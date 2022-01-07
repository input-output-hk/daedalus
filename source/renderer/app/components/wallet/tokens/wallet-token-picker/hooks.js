// @flow
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
  previousCheckedIds,
}: UseCheckboxes) => {
  const [checkboxes, setCheckboxes] = useState<BooleanMap>({});
  const checkedIds = useMemo(() => getCheckedIds(checkboxes), [checkboxes]);
  const disabledIdsSet = useMemo(() => new Set<string>(previousCheckedIds), [
    previousCheckedIds,
  ]);
  const currentAssetIds = useMemo(
    () => getEnabledAssetIds({ assets: currentAssets, disabledIdsSet }),
    [currentAssets, disabledIdsSet]
  );
  const currentCheckedIds = useMemo(
    () => getCurrentCheckedIds({ checkboxes, currentAssetIds }),
    [checkboxes, currentAssetIds]
  );
  const currentCheckedCount = currentCheckedIds.length;
  const totalCheckedCount = checkedIds.length + disabledIdsSet.size;
  const isMaxTotalCount =
    totalCheckedCount === Math.min(MAX_TOKENS, assets.length);
  const isMaxCurrentCount =
    currentCheckedCount === Math.min(MAX_TOKENS, currentAssetIds.length);
  const clearAll = useCallback(
    () => setCheckboxes(clearSelection({ checkboxes, currentAssetIds })),
    [setCheckboxes, checkboxes, currentAssetIds]
  );
  const checkMax = useCallback(() => {
    setCheckboxes(
      getMaxTokensIdMap({
        checkedIds,
        currentAssetIds,
        previousCheckedIds,
      })
    );
  }, [currentAssetIds, setCheckboxes, previousCheckedIds, checkedIds]);
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
  const toogleAllFn = isClearAllMode ? clearAll : checkMax;
  const toggleCheckbox = useCallback(
    (assetId: string) => {
      const newValue = !checkboxes[assetId];
      if (totalCheckedCount < MAX_TOKENS || !newValue) {
        setCheckboxes({
          ...checkboxes,
          [assetId]: newValue,
        });
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
    disabledIdsSet,
    toogleAllFn,
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
  }, [assets, searchValue, filterOption]);

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
    debounce(setPosition, 100, { leading: true }),
    [setPosition]
  );
  const onScroll = (evt: SyntheticMouseEvent<HTMLElement>) => {
    evt.persist();
    debouncedSetPosition(evt.target);
  };

  return {
    scrollPosition,
    onScroll,
  };
};
