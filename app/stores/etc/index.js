// @flow
import { observable, action } from 'mobx';
import EtcWalletsStore from './EtcWalletsStore';
import EtcWalletSettingsStore from './EtcWalletSettingsStore';

export const etcStoreClasses = {
  wallets: EtcWalletsStore,
  walletSettings: EtcWalletSettingsStore,
};

export type EtcStoresMap = {
  wallets: EtcWalletsStore,
  walletSettings: EtcWalletSettingsStore,
};

const etcStores = observable({
  wallets: null,
  walletSettings: null,
});

// Set up and return the stores and reset all stores to defaults
export default action((stores, api, actions): EtcStoresMap => {
  const storeNames = Object.keys(etcStoreClasses);
  storeNames.forEach(name => { if (etcStores[name]) etcStores[name].teardown(); });
  storeNames.forEach(name => {
    etcStores[name] = new etcStoreClasses[name](stores, api, actions);
  });
  storeNames.forEach(name => { if (etcStores[name]) etcStores[name].initialize(); });
  return etcStores;
});
