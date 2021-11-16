// @flow
import type { StorageKey } from '../../../../common/types/electron-store.types';

export const localStorageBridge = async () => {
  const isElectron = /electron/i.test(navigator.userAgent);

  if (isElectron) {
    const { default: LocalStorageApi } = await import('./localStorage');

    return {
      get: LocalStorageApi.get,
      set: LocalStorageApi.set,
      unset: LocalStorageApi.unset,
    };
  }

  return Promise.resolve({
    get: (key: StorageKey, defaultValue: any) =>
      Promise.resolve<any>(localStorage.getItem(key) || defaultValue),
    set: (key: StorageKey, value: any) =>
      Promise.resolve(localStorage.setItem(key, value)),
    unset: (key: StorageKey) => Promise.resolve(localStorage.removeItem(key)),
  });
};
