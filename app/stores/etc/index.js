// @flow
import { observable, action } from 'mobx';
import AccountsStore from './WalletsStore';

export const adaStoreClasses = {
  wallets: AccountsStore,
};

export type AdaStoresMap = {
  wallets: AccountsStore,
};

const adaStores = observable({
  wallets: null,
});

// Set up and return the stores and reset all stores to defaults
export default action((stores, api, actions): AdaStoresMap => {
  const storeNames = Object.keys(adaStoreClasses);
  storeNames.forEach(name => { if (adaStores[name]) adaStores[name].teardown(); });
  storeNames.forEach(name => {
    adaStores[name] = new adaStoreClasses[name](stores, api, actions);
  });
  storeNames.forEach(name => { if (adaStores[name]) adaStores[name].initialize(); });
  return adaStores;
});
