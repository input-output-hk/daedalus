// @flow

import React, { useState } from 'react';
import type { Node } from 'react';
import { merge } from 'lodash/fp';
import {
  getFeatureFromContext,
  useFeature,
} from '../../utils/mobx-features/hooks';
import { useLocalStorageFeature } from '../local-storage';

import { DiscreetMode } from './feature';
import { DiscreetModeApi } from './api';

export const discreetModeContext = React.createContext<DiscreetMode | null>(
  null
);

interface Props {
  children: Node;
}

export const DiscreetModeFeatureProvider = ({ children }: Props) => {
  const localStorageFeature = useLocalStorageFeature();
  const [discreetModeFeature] = useState<DiscreetMode>(() => {
    const feature = new DiscreetMode(new DiscreetModeApi(localStorageFeature));
    window.daedalus = merge(window.daedalus, {
      features: {
        discreetModeFeature: feature,
      },
    });

    return feature;
  });

  useFeature(discreetModeFeature);

  return (
    <discreetModeContext.Provider value={discreetModeFeature}>
      {children}
    </discreetModeContext.Provider>
  );
};

export function useDiscreetModeFeature(): DiscreetMode {
  return getFeatureFromContext(discreetModeContext);
}
