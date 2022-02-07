import React from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { LocalStorageFeatureProvider } from '../context';

type Props = {
  children: Node;
};
export function BrowserLocalStorageBridge({ children }: Props) {
  return (
    <LocalStorageFeatureProvider
      localStorage={{
        get: (key: string, defaultValue: any) =>
          Promise.resolve<any>(localStorage.getItem(key) || defaultValue),
        set: (key: string, value: any) =>
          Promise.resolve(localStorage.setItem(key, value)),
        unset: (key: string) => Promise.resolve(localStorage.removeItem(key)),
      }}
    >
      {children}
    </LocalStorageFeatureProvider>
  );
}
