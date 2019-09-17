// @flow

/**
 * This api layer provides access to the electron local storage
 * for wallet properties that are not synced with the ADA backend.
 */

type WalletLocalData = {
  id: string,
  mnemonicsConfirmationDate?: ?Date,
};

export default class WalletLocalStorage {
  constructor(electronStore: Object, network: string) {
    this.electronStore = electronStore;
    const networkForLocalStorage = String(network);
    this.storageKeys = {
      WALLETS: `${networkForLocalStorage}-WALLETS`,
    };
  }

  getWalletLocalData = (walletId: string): Promise<WalletLocalData> =>
    new Promise((resolve, reject) => {
      try {
        let walletData = this.electronStore.get(
          `${this.storageKeys.WALLETS}.${walletId}`
        );
        if (!walletData) {
          walletData = {
            id: walletId,
          };
          this.setWalletLocalData(walletData);
        }
        return resolve(walletData);
      } catch (error) {
        return reject(error);
      }
    });

  setWalletLocalData = (walletData: WalletLocalData): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        const walletId = walletData.id;
        this.electronStore.set(
          `${this.storageKeys.WALLETS}.${walletId}`,
          walletData
        );
        return resolve();
      } catch (error) {
        return reject(error);
      }
    });

  updateWalletLocalData = (updatedWalletData: { id: string }): Promise<void> =>
    new Promise(async (resolve, reject) => {
      const walletId = updatedWalletData.id;
      const walletData = await this.getWalletLocalData(walletId);
      Object.assign(walletData, updatedWalletData);
      try {
        this.electronStore.set(
          `${this.storageKeys.WALLETS}.${walletId}`,
          walletData
        );
        return resolve();
      } catch (error) {
        return reject(error);
      }
    });

  unsetWalletLocalData = (walletId: string): Promise<void> =>
    new Promise((resolve, reject) => {
      try {
        this.electronStore.delete(`${this.storageKeys.WALLETS}.${walletId}`);
        return resolve();
      } catch (error) {
        return reject(error);
      }
    });
}
