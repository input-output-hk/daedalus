import { app } from 'electron';
import { logger } from './logging';
import { LOCALES } from '../../common/types/locales.types';
import type { Locale } from '../../common/types/locales.types';

export const detectSystemLocale = (): Locale => {
  const systemLocale = app.getLocale();
  logger.info('Detected system locale', {
    systemLocale,
  });

  if (systemLocale === 'ja') {
    return LOCALES.japanese;
  }

  return LOCALES.english;
};
