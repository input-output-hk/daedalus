import React, { useEffect, useState } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { merge } from 'lodash/fp';
import {
  getFeatureFromContext,
  useFeature,
} from '../../utils/mobx-features/hooks';
import { useLocalStorageFeature } from '../local-storage';
import { DiscreetMode } from './feature';
import { DiscreetModeApi } from './api';
import { useAnalytics } from '../../components/analytics';

export const discreetModeContext = React.createContext<DiscreetMode | null>(
  null
);
interface Props {
  children: Node;
}

export function DiscreetModeFeatureProvider({ children }: Props) {
  const localStorageFeature = useLocalStorageFeature();
  const analyticsClient = useAnalytics();

  const [discreetModeFeature] = useState<DiscreetMode>(() => {
    const feature = new DiscreetMode(
      new DiscreetModeApi(localStorageFeature),
      analyticsClient
    );

    window.daedalus = merge(window.daedalus, {
      features: {
        discreetModeFeature: feature,
      },
    });

    return feature;
  });

  useEffect(() => {
    if (analyticsClient) {
      discreetModeFeature.setAnalyticsClient(analyticsClient);
    }
  }, [analyticsClient]);

  useFeature(discreetModeFeature);

  return (
    <discreetModeContext.Provider value={discreetModeFeature}>
      {children}
    </discreetModeContext.Provider>
  );
}
export function useDiscreetModeFeature(): DiscreetMode {
  return getFeatureFromContext(discreetModeContext);
}
