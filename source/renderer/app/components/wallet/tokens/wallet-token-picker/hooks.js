// @flow
import { useState, useRef, useEffect, useCallback } from 'react';
import { debounce } from 'lodash';
import { ScrollPositionEnum, getScrollPosition } from './helpers';
import type { ScrollPosition } from './helpers';

export const useSearch = () => {
  const [searchValue, setSearchValue] = useState<string>('');

  return {
    searchValue,
    setSearchValue,
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
  const debounced = useCallback(debounce(onScroll, 100, { leading: true }), []);

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
