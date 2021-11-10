// @flow

import React, { useState } from 'react';
import type { Node } from 'react';
import {
  getFeatureFromContext,
  useFeature,
} from '../../utils/mobx-features/hooks';

import { DiscreetMode } from './feature';

export const discreetModeContext = React.createContext<DiscreetMode | null>(
  null
);

interface Props {
  children: Node;
}

export const DiscreetModeFeatureProvider = ({ children }: Props) => {
  const [discreetModeFeature] = useState<DiscreetMode>(new DiscreetMode());

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
