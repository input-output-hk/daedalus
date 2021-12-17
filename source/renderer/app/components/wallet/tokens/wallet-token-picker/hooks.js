// @flow
import { useState, useRef, useEffect, useCallback } from 'react';
import { debounce } from 'lodash';
import {
  ScrollPositionEnum,
  getScrollPosition,
  maxTokensArrayToIdMap,
  MAX_TOKENS,
} from './helpers';
import type { ScrollPosition, UseCheckboxes, CheckBoxes } from './types';

export const useSearch = () => {
  const [searchValue, setSearchValue] = useState<string>('');

  return {
    searchValue,
    setSearchValue,
  };
};

export const useCheckboxes = ({ assets }: UseCheckboxes) => {
  const [checkboxes, setCheckboxes] = useState<CheckBoxes>({});
  const checkedCount = Object.values(checkboxes).filter(Boolean).length;

  return {
    checkboxes,
    checkedCount,
    checkFirst30: () => setCheckboxes(() => maxTokensArrayToIdMap(assets)),
    toggleCheckbox: (assetId: string) => {
      const newValue = !checkboxes[assetId];
      if (checkedCount < MAX_TOKENS || !newValue) {
        setCheckboxes({
          ...checkboxes,
          [assetId]: newValue,
        });
      }
    },
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
