// @flow
import { useState, useEffect, useCallback, useMemo } from 'react';
import { debounce } from 'lodash';
import { searchAssets } from '../../../../utils/assets';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
} from './const';
import {
  getAssetIds,
  getCheckedIds,
  filterAssets,
  getScrollPosition,
  getMaxTokensIdsMap,
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
  previousCheckedIds,
}: UseCheckboxes) => {
  const [checkboxes, setCheckboxes] = useState<BooleanMap>({});
  const assetIds = useMemo(() => getAssetIds(assets), [assets]);
  const checkedIds = useMemo(() => getCheckedIds(checkboxes), [checkboxes]);
  const disabledIdsSet = useMemo(() => new Set<string>(previousCheckedIds), [
    previousCheckedIds,
  ]);
  const checkedCount = checkedIds.length + disabledIdsSet.size;
  const check30First = useCallback(() => {
    setCheckboxes(
      getMaxTokensIdsMap({
        assetIds,
        previousCheckedIds,
      })
    );
  }, [assetIds, previousCheckedIds]);
  const toggleCheckbox = useCallback(
    (assetId: string) => {
      const newValue = !checkboxes[assetId];
      if (checkedCount < MAX_TOKENS || !newValue) {
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
    checkedCount,
    checkedIds,
    disabledIdsSet,
    check30First,
    toggleCheckbox,
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
  const onScroll = (evt: Event) => {
    evt.persist();
    debouncedSetPosition(evt.target);
  };

  return {
    scrollPosition,
    onScroll,
  };
};
