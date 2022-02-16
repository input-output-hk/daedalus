import { StorageKey } from '../../../../common/types/electron-store.types';

export interface LocalStorage {
  get: (key: StorageKey, defaultValue?: unknown) => Promise<unknown>;
  set: (key: StorageKey, value: unknown) => Promise<unknown>;
  unset: (key: StorageKey) => Promise<void>;
}
