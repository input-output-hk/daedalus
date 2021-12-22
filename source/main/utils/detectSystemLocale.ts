// @flow
import { app } from 'electron';
import { logger } from './logging';
import { LOCALES } from '../../common/types/locales.types.js';
import type { Locale } from '../../common/types/locales.types.js';

export const detectSystemLocale = (): Locale => {
  const systemLocale = app.getLocale();
  logger.info('Detected system locale', { systemLocale });
  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }
  return LOCALES.english;
};
