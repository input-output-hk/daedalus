// @flow
import { app } from 'electron';
import Store from 'electron-store';
import { DEFAULT_LOCALES } from '../../common/types/environment.types.js';

const store = new Store();

export const getDefaultLocale = (): string => {
  const detectedLocale = app.getLocale();
  if (detectedLocale === 'ja') { return DEFAULT_LOCALES.japanese; }
  return DEFAULT_LOCALES.english;
};

export const getLocale = (network: string) => {
  const defaultLocale = getDefaultLocale();
  try {
    const locale = store.get(`${network}-USER-LOCALE`);
    if (locale) { return locale; }
    return defaultLocale;
  } catch (error) {
    return defaultLocale;
  }
};
