// @ts-nocheck
import type { ElementRef } from 'react';
import { useEffect, useState, useCallback } from 'react';
export function useDebouncedValueChangedIndicator(value: any, delay: number) {
  const [isDirty, setIsDirty] = useState(false);
  useEffect(() => {
    if (value === '') {
      setIsDirty(false);
    }

    // Set debouncedValue to value (passed in) after the specified delay
    const handler = setTimeout(() => {
      if (value !== '') {
        setIsDirty(true);
      }
    }, delay);
    return () => {
      clearTimeout(handler);
    };
  }, [value]);
  return isDirty;
}

/**
 * Hook for setting a ref and re-rendering when the value changes
 * Inspired by https://medium.com/@teh_builder/ref-objects-inside-useeffect-hooks-eb7c15198780
 */
export function manageRef(ref: ElementRef<any>) {
  return useCallback((node) => {
    ref.current = node;
  }, []);
}

/**
 * Hook for handling the on/off state of events that
 * are toggle-able like "hovered" or "focused"
 */
export function handleRefState(
  ref: ElementRef<any>,
  events: {
    on: string;
    off: string;
  }
) {
  const [isOn, setIsOn] = useState(false);

  const onListener = () => setIsOn(true);

  const offListener = () => setIsOn(false);

  useEffect(() => {
    if (ref.current) {
      const { current } = ref;
      current.addEventListener(events.on, onListener);
      current.addEventListener(events.off, offListener);
    }

    // Remove event listeners on component unmount
    return () => {
      if (ref.current) {
        const { current } = ref;
        current.removeEventListener(events.on, onListener);
        current.removeEventListener(events.off, offListener);
      }
    };
  });
  return isOn;
}
