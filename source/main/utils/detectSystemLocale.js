// @flow
import { app } from 'electron';
import { LOCALES } from '../../common/types/locales.types.js';
import type { Locale } from '../../common/types/locales.types.js';

export const detectSystemLocale = (): Locale => {
  const systemLocale = app.getLocale();
  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }
  return LOCALES.english;
};
