// @flow
import Store from 'electron-store';

const store = new Store();

export const getLocale = (network: string) => {
  const english = 'en-US';
  try {
    const locale = store.get(`${network}-USER-LOCALE`);
    if (locale) {
      return locale;
    }
    return english;
  } catch (error) {
    return english;
  }
};
