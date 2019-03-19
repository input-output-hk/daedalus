// @flow
import { app } from 'electron';
import { Logger } from './logging';
import { LOCALES } from '../../common/types/locales.types.js';

export const detectSystemLocale = (): string => {
  const systemLocale = app.getLocale();
  const systemLang = process.env.LANG || 'unknown';
  Logger.info('Detected system locale', { systemLocale, systemLang });
  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }
  return LOCALES.english;
};
