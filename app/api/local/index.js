import localStorage from "electron-json-storage";

/**
 * This api layer provides access to the electron local storage
 * for user settings that are not synced with any coin backend.
 */

export default class LocalApi {

  getUserLocale= () => new Promise((resolve, reject) => {
    localStorage.get('userLocale', (error, response) => {
      if (error) return reject(error);
      if (!response.locale) return resolve('');
      resolve(response.locale);
    });
  });

  setUserLocale = (locale: string) => new Promise((resolve, reject) => {
    localStorage.set('userLocale', { locale }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetUserLocale = () => new Promise((resolve) => {
    localStorage.remove('userLocale', () => {
      resolve();
    });
  });

  getTermsOfUseAcceptance = () => new Promise((resolve, reject) => {
    localStorage.get('termsOfUseAcceptance', (error, response) => {
      if (error) return reject(error);
      if (!response.accepted) return resolve(false);
      resolve(response.accepted);
    });
  });

  setTermsOfUseAcceptance = () => new Promise((resolve, reject) => {
    localStorage.set('termsOfUseAcceptance', { accepted: true }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetTermsOfUseAcceptance = () => new Promise((resolve) => {
    localStorage.remove('termsOfUseAcceptance', () => {
      resolve();
    });
  });

  getSendLogsChoice = () => new Promise((resolve, reject) => {
    localStorage.get('sendLogsChoice', (error, response) => {
      if (error) return reject(error);
      if (typeof response.sendLogs === 'undefined') {
        return resolve(null);
      }
      resolve(response.sendLogs);
    });
  });

  setSendLogsChoice = (sendLogs: boolean) => new Promise((resolve, reject) => {
    localStorage.set('sendLogsChoice', { sendLogs }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetSendLogsChoice = () => new Promise((resolve) => {
    localStorage.remove('sendLogsChoice', () => {
      resolve();
    });
  });

  getUserTheme = () => new Promise((resolve, reject) => {
    localStorage.get('theme', (error, response) => {
      if (error) return reject(error);
      if (!response.theme) return resolve('');
      resolve(response.theme);
    });
  });

  setUserTheme = (theme: string) => new Promise((resolve, reject) => {
    localStorage.set('theme', { theme }, (error) => {
      if (error) return reject(error);
      resolve();
    });
  });

  unsetUserTheme = () => new Promise((resolve) => {
    localStorage.remove('theme', () => {
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
