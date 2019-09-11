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

// ======= DUMMY DATA =======

export const ETC_WALLETS_DATA = [
  {
    id: '0xafe149dce151dc829008779820cc4a947ab2257e',
    name: 'Wallet 1',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0x6f71adcdb471af7f7623d7e4ff27d9e5de4fd3b1',
    name: 'Wallet 2',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0xde81f296a5c102ce0a4ac2b98f203f6763895185',
    name: 'Wallet 3',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0xbe1f71291cb65c6aadd025427e9f60e8f9c95ffc',
    name: 'Wallet 4',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0x31d3500eaff3f8e6a3cfa1ab0523abe2e7910424',
    name: 'Wallet 5',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
  {
    id: '0xf5c12ec7f63d6a134366c886bcf9e70777107fd8',
    name: 'Wallet 6',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0xa492e4770faf26399df9cfb3f1c07f08cd69a3f1',
    name: 'Wallet 7',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0x37c0762a7b9dad03d7934353abf28daa31947e4e',
    name: 'Wallet 8',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0x7864deb4fe7808eb09cb11c4b22effc21b75dc05',
    name: 'Wallet 9',
    assurance: 'CWANormal',
    hasPassword: true,
    passwordUpdateDate: new Date('2017-10-01'),
  },
  {
    id: '0x0a61652996de1b8051cc2b0dfc6e671ac5f4e624',
    name: 'Wallet 10',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
  {
    id: '0x2ba43ca4dc0788ae2b09a3e75e4c1ff191a84279',
    name: 'Wallet 11',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
  {
    id: '0x9a62039f33d6ade11d00a127a30ba5fee4c969cb',
    name: 'Wallet 12',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
  {
    id: '0xf39c9ed4abd09e18abc3ba40a53d7c65e2e6c25f',
    name: 'Wallet 13',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
  {
    id: '0x2fecc6a10ab19719f7a4cd493e018742d119f945',
    name: 'Wallet 14',
    assurance: 'CWANormal',
    hasPassword: false,
    passwordUpdateDate: null,
  },
];

export const initEtcWalletsDummyData = (): Promise<void> =>
  new Promise(async (resolve, reject) => {
    try {
      ETC_WALLETS_DATA.forEach(async dummyWalletData => {
        const walletId = dummyWalletData.id;
        const walletData = await getWalletLocalData(walletId);
        if (!walletData) await setWalletLocalData(dummyWalletData);
      });
      return resolve();
    } catch (error) {
      return reject(error);
    }
  });
