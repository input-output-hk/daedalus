import React, { useState } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
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
    // @ts-ignore ts-migrate(2339) FIXME: Property 'daedalus' does not exist on type 'Window... Remove this comment to see the full error message
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
