// @flow
import { observable, action } from 'mobx';
import WalletsStore from './WalletsStore';

export const etcStoreClasses = {
  wallets: WalletsStore,
};

export type EtcStoresMap = {
  wallets: WalletsStore,
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
