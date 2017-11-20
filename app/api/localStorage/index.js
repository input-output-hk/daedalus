import localStorage from 'electron-json-storage';
import environment from '../../environment';

const networkForLocalStorage = String(environment.NETWORK);
const localStorageKeys = {
  USER_LOCALE: networkForLocalStorage + '-USER-LOCALE',
  TERMS_OF_USE_ACCEPTANCE: networkForLocalStorage + '-TERMS-OF-USE-ACCEPTANCE',
  SEND_LOGS_CHOICE: networkForLocalStorage + '-SEND-LOGS-CHOICE',
  THEME: networkForLocalStorage + '-THEME'
};

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalStorageApi {

  getUserLocale = () => new Promise((resolve, reject) => {
    localStorage.get(localStorageKeys.USER_LOCALE, (error, response) => {
      if (error) return reject(error);
      if (!response.locale) return resolve('');
      resolve(response.locale);
    });
  });

  setUserLocale = (locale: string) => new Promise((resolve, reject) => {
    localStorage.set(localStorageKeys.USER_LOCALE, { locale }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetUserLocale = () => new Promise((resolve) => {
    localStorage.remove(localStorageKeys.USER_LOCALE, () => {
      resolve();
    });
  });

  getTermsOfUseAcceptance = () => new Promise((resolve, reject) => {
    localStorage.get(localStorageKeys.TERMS_OF_USE_ACCEPTANCE, (error, response) => {
      if (error) return reject(error);
      if (!response.accepted) return resolve(false);
      resolve(response.accepted);
    });
  });

  setTermsOfUseAcceptance = () => new Promise((resolve, reject) => {
    localStorage.set(localStorageKeys.TERMS_OF_USE_ACCEPTANCE, { accepted: true }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetTermsOfUseAcceptance = () => new Promise((resolve) => {
    localStorage.remove(localStorageKeys.TERMS_OF_USE_ACCEPTANCE, () => {
      resolve();
    });
  });

  getSendLogsChoice = () => new Promise((resolve, reject) => {
    localStorage.get(localStorageKeys.SEND_LOGS_CHOICE, (error, response) => {
      if (error) return reject(error);
      if (typeof response.sendLogs === 'undefined') {
        return resolve(null);
      }
      resolve(response.sendLogs);
    });
  });

  setSendLogsChoice = (sendLogs: boolean) => new Promise((resolve, reject) => {
    localStorage.set(localStorageKeys.SEND_LOGS_CHOICE, { sendLogs }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetSendLogsChoice = () => new Promise((resolve) => {
    localStorage.remove(localStorageKeys.SEND_LOGS_CHOICE, () => {
      resolve();
    });
  });

  getUserTheme = () => new Promise((resolve, reject) => {
    localStorage.get(localStorageKeys.THEME, (error, response) => {
      if (error) return reject(error);
      if (!response.theme) return resolve('');
      resolve(response.theme);
    });
  });

  setUserTheme = (theme: string) => new Promise((resolve, reject) => {
    localStorage.set(localStorageKeys.THEME, { theme }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetUserTheme = () => new Promise((resolve) => {
    localStorage.remove(localStorageKeys.THEME, () => {
      resolve();
    });
  });

  async reset() {
    await this.unsetUserLocale(); // TODO: remove after saving locale to API is restored
    await this.unsetTermsOfUseAcceptance();
    await this.unsetSendLogsChoice();
    await this.unsetUserTheme();
  }

}
