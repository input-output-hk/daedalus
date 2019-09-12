// @flow

const { environment } = global;
let { electronStore } = global;
const { network } = environment;

const networkForLocalStorage = String(network);
const storageKeys = {
  WALLETS: `${networkForLocalStorage}-WALLETS`,
};

// TODO: Find a better way to handle situation where ElectronStore is not available
if (!electronStore) {
  electronStore = {
    get: localStorage.getItem.bind(localStorage),
    set: localStorage.setItem.bind(localStorage),
    delete: localStorage.removeItem.bind(localStorage),
  };
}

/**
 * This api layer provides access to the electron local storage
 * for wallet properties that are not synced with the ADA backend.
 */

export type WalletLocalData = {
  id: string,
  mnemonicsConfirmationDate?: ?Date,
};

export const getWalletLocalData = (
  walletId: string
): Promise<WalletLocalData> =>
  new Promise((resolve, reject) => {
    try {
      let walletData = electronStore.get(`${storageKeys.WALLETS}.${walletId}`);
      if (!walletData) {
        walletData = {
          id: walletId,
        };
        setWalletLocalData(walletData);
      }
      return resolve(walletData);
    } catch (error) {
      return reject(error);
    }
  });

export const setWalletLocalData = (
  walletData: WalletLocalData
): Promise<void> =>
  new Promise((resolve, reject) => {
    try {
      const walletId = walletData.id;
      electronStore.set(`${storageKeys.WALLETS}.${walletId}`, walletData);
      return resolve();
    } catch (error) {
      return reject(error);
    }
  });

export const updateWalletLocalData = (updatedWalletData: {
  id: string,
}): Promise<void> =>
  new Promise(async (resolve, reject) => {
    const walletId = updatedWalletData.id;
    const walletData = await getWalletLocalData(walletId);
    Object.assign(walletData, updatedWalletData);
    try {
      electronStore.set(`${storageKeys.WALLETS}.${walletId}`, walletData);
      return resolve();
    } catch (error) {
      return reject(error);
    }
  });

export const unsetWalletLocalData = (walletId: string): Promise<void> =>
  new Promise((resolve, reject) => {
    try {
      electronStore.delete(`${storageKeys.WALLETS}.${walletId}`);
      return resolve();
    } catch (error) {
      return reject(error);
    }
  });

export const unsetEtcWalletsData = (): Promise<void> =>
  new Promise((resolve, reject) => {
    try {
      electronStore.delete(storageKeys.WALLETS);
      return resolve();
    } catch (error) {
      return reject(error);
    }
  });
