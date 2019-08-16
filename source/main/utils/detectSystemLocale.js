// @flow
import { app } from 'electron';
import { Logger } from './logging';
import { LOCALES } from '../../common/types/locales.types.js';

export const detectSystemLocale = (): string => {
  const systemLocale = app.getLocale();
  Logger.info('Detected system locale', { systemLocale });
  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }
  if (systemLocale === 'en-US') {
    return LOCALES.english;
  }
  return systemLocale;
};

export const detectSystemDateLocale = (): string => {
  const systemDateLocale = app.getLocale();
  return systemDateLocale;
};
