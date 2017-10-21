// @flow
import localStorage from 'electron-json-storage';

/**
 * This api layer provides access to the electron local storage
 * for account/wallet properties that are not synced with ETC backend.
 */

export const getWalletName = (walletId: string) => new Promise((resolve, reject) => {
  localStorage.get('walletNames', (error, response) => {
    if (error) return reject(error);
    if (!response[walletId]) return resolve('Untitled Wallet');
    resolve(response[walletId]);
  });
});

export const setWalletName = (
  walletId: string, walletName: string
) => new Promise((resolve, reject) => {
  localStorage.set('walletNames', { [walletId]: walletName }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

export const unsetWalletNames = () => new Promise((resolve) => {
  localStorage.remove('walletNames', () => {
    resolve();
  });
});
