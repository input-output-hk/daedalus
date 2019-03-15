// @flow
import { app } from 'electron';
import { LOCALES } from '../../common/types/locales.types.js';

export const detectSystemLocale = (): string => {
  const systemLocale = app.getLocale();
  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }
  return LOCALES.english;
};
