import { useContext, useEffect, useLayoutEffect } from 'react';
import type { Context } from 'react';
import { Feature } from './feature';

/**
 * Manages the lifecycle of a feature based on its provider component.
 *
 * 1. Starts the feature store when the enclosing component mounts
 * 2. Stops the store on unmount
 * @param feature
 */
export const useFeature = (feature: Feature) => {
  useLayoutEffect(() => {
    // Start feature store before first render is done
    feature.start();
  }, []);
  useEffect(() => {
    // Stop store on unmount
    return () => {
      feature.stop();
    };
  }, []);
};

/**
 * Throws an error if the feature context hasn't been provided
 * before a component tries to use the feature.
 *
 * @param context
 */
export function getFeatureFromContext<T>(context: Context<T | null>): T {
  const instance = useContext(context);

  if (!instance) {
    throw new Error(`You need to provide a context before using it.`);
  }

  return instance;
}
