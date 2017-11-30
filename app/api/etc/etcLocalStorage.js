// @flow
import localStorage from 'electron-json-storage';
import { set, unset } from 'lodash';
import type { AssuranceModeOption } from '../../types/transactionAssuranceTypes';
import environment from '../../environment';

const networkForLocalStorage = String(environment.NETWORK);
const localStorageKeys = {
  WALLETS: networkForLocalStorage + '-ETC-WALLETS',
};

/**
 * This api layer provides access to the electron local storage
 * for account/wallet properties that are not synced with ETC backend.
 */

export type EtcWalletData = {
  id: string,
  name: string,
  assurance: AssuranceModeOption,
  hasPassword: boolean,
  passwordUpdateDate: ?Date,
};

export type EtcWalletsData = {
  wallets: Array<EtcWalletData>,
};

export const getEtcWalletsData = (): Promise<EtcWalletsData> => new Promise((resolve, reject) => {
  localStorage.get(localStorageKeys.WALLETS, (error, response) => {
    if (error) return reject(error);
    if (!response.wallets) return resolve({ wallets: [] });
    resolve(response.wallets);
  });
});

export const setEtcWalletsData = (
  walletsData: Array<EtcWalletData>
): Promise<void> => new Promise((resolve, reject) => {
  const wallets = {};
  walletsData.forEach(walletData => {
    wallets[walletData.id] = walletData;
  });
  localStorage.set(localStorageKeys.WALLETS, { wallets }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

export const getEtcWalletData = (
  walletId: string
): Promise<EtcWalletData> => new Promise(async (resolve) => {
  const walletsData = await getEtcWalletsData();
  resolve(walletsData[walletId]);
});

export const setEtcWalletData = (
  walletData: EtcWalletData
): Promise<void> => new Promise(async (resolve, reject) => {
  const walletsData = await getEtcWalletsData();
  set(walletsData, walletData.id, walletData);
  localStorage.set(localStorageKeys.WALLETS, { wallets: walletsData }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

export const updateEtcWalletData = (
  walletData: {
    id: string,
    name?: string,
    assurance?: AssuranceModeOption,
    hasPassword?: boolean,
    passwordUpdateDate?: ?Date,
  }
): Promise<void> => new Promise(async (resolve, reject) => {
  const walletsData = await getEtcWalletsData();
  const walletId = walletData.id;
  Object.assign(walletsData[walletId], walletData);
  localStorage.set(localStorageKeys.WALLETS, { wallets: walletsData }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

export const unsetEtcWalletData = (
  walletId: string
): Promise<void> => new Promise(async (resolve, reject) => {
  const walletsData = await getEtcWalletsData();
  unset(walletsData, walletId);
  localStorage.set(localStorageKeys.WALLETS, { wallets: walletsData }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});

export const unsetEtcWalletsData = (): Promise<void> => new Promise((resolve) => {
  localStorage.remove(localStorageKeys.WALLETS, () => {
    resolve();
  });
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

export const initEtcWalletsDummyData = (): Promise<void> => new Promise(async (resolve, reject) => {
  const wallets = await getEtcWalletsData();
  ETC_WALLETS_DATA.forEach(walletData => {
    set(wallets, walletData.id, walletData);
  });
  localStorage.set(localStorageKeys.WALLETS, { wallets }, (error) => {
    if (error) return reject(error);
    resolve();
  });
});
