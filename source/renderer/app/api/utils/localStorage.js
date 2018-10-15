// @flow
import Store from 'electron-store';
import environment from '../../../../common/environment';

const store = new Store();

const networkForLocalStorage = String(environment.NETWORK);
const storageKeys = {
  USER_LOCALE: networkForLocalStorage + '-USER-LOCALE',
  TERMS_OF_USE_ACCEPTANCE: networkForLocalStorage + '-TERMS-OF-USE-ACCEPTANCE',
  THEME: networkForLocalStorage + '-THEME',
  DATA_LAYER_MIGRATION_ACCEPTANCE: networkForLocalStorage + '-DATA-LAYER-MIGRATION-ACCEPTANCE',
};

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalStorageApi {

  getUserLocale = (): Promise<string> => new Promise((resolve, reject) => {
    try {
      const locale = store.get(storageKeys.USER_LOCALE);
      if (!locale) return resolve('');
      resolve(locale);
    } catch (error) {
      return reject(error);
    }
  });

  setUserLocale = (locale: string): Promise<void> => new Promise((resolve, reject) => {
    try {
      store.set(storageKeys.USER_LOCALE, locale);
      resolve();
    } catch (error) {
      return reject(error);
    }
  });

  unsetUserLocale = (): Promise<void> => new Promise((resolve) => {
    try {
      store.delete(storageKeys.USER_LOCALE);
      resolve();
    } catch (error) {} // eslint-disable-line
  });

  getTermsOfUseAcceptance = (): Promise<boolean> => new Promise((resolve, reject) => {
    try {
      const accepted = store.get(storageKeys.TERMS_OF_USE_ACCEPTANCE);
      if (!accepted) return resolve(false);
      resolve(accepted);
    } catch (error) {
      return reject(error);
    }
  });

  setTermsOfUseAcceptance = (): Promise<void> => new Promise((resolve, reject) => {
    try {
      store.set(storageKeys.TERMS_OF_USE_ACCEPTANCE, true);
      resolve();
    } catch (error) {
      return reject(error);
    }
  });

  unsetTermsOfUseAcceptance = (): Promise<void> => new Promise((resolve) => {
    try {
      store.delete(storageKeys.TERMS_OF_USE_ACCEPTANCE);
      resolve();
    } catch (error) {} // eslint-disable-line
  });

  getUserTheme = (): Promise<string> => new Promise((resolve, reject) => {
    try {
      const theme = store.get(storageKeys.THEME);
      if (!theme) return resolve('');
      resolve(theme);
    } catch (error) {
      return reject(error);
    }
  });

  setUserTheme = (theme: string): Promise<void> => new Promise((resolve, reject) => {
    try {
      store.set(storageKeys.THEME, theme);
      resolve();
    } catch (error) {
      return reject(error);
    }
  });

  unsetUserTheme = (): Promise<void> => new Promise((resolve) => {
    try {
      store.delete(storageKeys.THEME);
      resolve();
    } catch (error) {} // eslint-disable-line
  });

  getDataLayerMigrationAcceptance = (): Promise<boolean> => new Promise((resolve, reject) => {
    try {
      const accepted = store.get(storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE);
      if (!accepted) return resolve(false);
      resolve(true);
    } catch (error) {
      return reject(error);
    }
  });

  setDataLayerMigrationAcceptance = (): Promise<void> => new Promise((resolve, reject) => {
    try {
      store.set(storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE, true);
      resolve();
    } catch (error) {
      return reject(error);
    }
  });

  unsetDataLayerMigrationAcceptance = (): Promise<void> => new Promise((resolve) => {
    try {
      store.delete(storageKeys.DATA_LAYER_MIGRATION_ACCEPTANCE);
      resolve();
    } catch (error) {} // eslint-disable-line
  });

  async reset() {
    await this.unsetUserLocale();
    await this.unsetTermsOfUseAcceptance();
    await this.unsetUserTheme();
    await this.unsetDataLayerMigrationAcceptance();
  }

}
