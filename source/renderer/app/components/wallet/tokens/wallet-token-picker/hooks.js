// @flow
import { useState, useRef, useEffect, useCallback } from 'react';
import { debounce } from 'lodash';
import { searchAssets } from '../../../../utils/assets';
import {
  MAX_TOKENS,
  ScrollPositionEnum,
  FilterSelectOptionsEnum,
} from './const';
import {
  filterAssets,
  getScrollPosition,
  maxTokensArrayToIdMap,
} from './helpers';
import type {
  Assets,
  FilterSelectOptions,
  ScrollPosition,
  UseCheckboxes,
  UseFilters,
  CheckBoxes,
} from './types';

export const useCheckboxes = ({ assets }: UseCheckboxes) => {
  const [checkboxes, setCheckboxes] = useState<CheckBoxes>({});
  const checkedCheckboxes = Object.entries(checkboxes).reduce(
    (acc: Array<string>, [uniqueId, checked]) =>
      checked ? [...acc, uniqueId] : acc,
    []
  );

  return {
    checkboxes,
    checkedCheckboxes,
    check30First: () => setCheckboxes(() => maxTokensArrayToIdMap(assets)),
    toggleCheckbox: (assetId: string) => {
      const newValue = !checkboxes[assetId];
      if (checkedCheckboxes.length < MAX_TOKENS || !newValue) {
        setCheckboxes({
          ...checkboxes,
          [assetId]: newValue,
        });
      }
    },
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
  const scrollableRef = useRef<HTMLElement | null>(null);
  const [scrollPosition, setScrollPosition] = useState<ScrollPosition>(
    ScrollPositionEnum.TOP
  );
  const onScroll = (evt: Event) => {
    const position: ScrollPosition = getScrollPosition(evt.target);
    setScrollPosition(position);
  };
  const debounced = useCallback(debounce(onScroll, 50, { leading: true }), []);

  useEffect(() => {
    scrollableRef?.current?.addEventListener('scroll', debounced);
    return () => {
      scrollableRef?.current?.removeEventListener('scroll', debounced);
    };
  }, [scrollableRef.current]);

  return {
    scrollableRef,
    scrollPosition,
  };
};
