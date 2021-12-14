// @flow

import React, { useState } from 'react';
import type { Node } from 'react';
import { merge } from 'lodash/fp';
import { getFeatureFromContext } from '../../utils/mobx-features/hooks';

import type { LocalStorageApi } from './types';

export const localStorageContext = React.createContext<LocalStorageApi | null>(
  null
);

interface Props {
  children: Node;
  localStorage: LocalStorageApi;
}

export const LocalStorageFeatureProvider = ({
  children,
  localStorage,
}: Props) => {
  const [localStorageFeature] = useState<LocalStorageApi>(() => {
    window.daedalus = merge(window.daedalus, {
      features: {
        localStorage,
      },
    });

    return localStorage;
  });

  return (
    <localStorageContext.Provider value={localStorageFeature}>
      {children}
    </localStorageContext.Provider>
  );
};

export function useLocalStorageFeature(): LocalStorageApi {
  return getFeatureFromContext(localStorageContext);
}
