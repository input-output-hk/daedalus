// @flow

export type LocalStorageApi = {
  get: (key: StorageKey, defaultValue: any) => boolean,
  set: (key: StorageKey, value: any) => Promise<void>,
  unset: (key: StorageKey) => Promise<void>,
};
