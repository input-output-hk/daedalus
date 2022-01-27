import Store from 'electron-store';
import { detectSystemLocale } from './detectSystemLocale';

const store = new Store();
export const getLocale = (network: string) => {
  const systemLocale = detectSystemLocale();

  try {
    const locale = store.get(`${network}-USER-LOCALE`);

    if (locale) {
      return locale;
    }

    return systemLocale;
  } catch (error) {
    return systemLocale;
  }
};
