import { LocalStorage } from './index';

export const browserLocalStorage: LocalStorage = {
  get: (key: string, defaultValue: unknown) =>
    Promise.resolve(localStorage.getItem(key) || defaultValue),
  set: (key: string, value: string) => {
    localStorage.setItem(key, value);
    return Promise.resolve();
  },
  unset: (key: string) => {
    localStorage.removeItem(key);
    return Promise.resolve();
  },
};
