import type { StorageKey } from '../../../../common/types/electron-store.types';

export type LocalStorageApi = {
  get: (key: StorageKey, defaultValue: any) => Promise<any>;
  set: (key: StorageKey, value: any) => Promise<void>;
  unset: (key: StorageKey) => Promise<void>;
};
