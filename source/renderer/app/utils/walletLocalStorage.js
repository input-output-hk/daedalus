// @flow

const { electronStore, environment } = global;
const { network } = environment;

const networkForLocalStorage = String(network);
const storageKeys = {
  WALLETS: `${networkForLocalStorage}-WALLETS`,
};

/**
 * This api layer provides access to the electron local storage
 * for wallet properties that are not synced with the ADA backend.
 */

export type WalletLocalData = {
  id: string,
  mnemonicsConfirmationDate: ?Date,
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
  mnemonicsConfirmationDate: Date,
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
