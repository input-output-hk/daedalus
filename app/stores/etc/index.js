// @flow
import { observable, action } from 'mobx';
import EtcWalletsStore from './EtcWalletsStore';

export const etcStoreClasses = {
  wallets: EtcWalletsStore,
};

export type EtcStoresMap = {
  wallets: EtcWalletsStore,
};

const etcStores = observable({
  wallets: null,
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
