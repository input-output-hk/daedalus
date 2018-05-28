// @flow
import { observable, action } from 'mobx';
import AdaWalletsStore from './AdaWalletsStore';
import TransactionsStore from './AdaTransactionsStore';
import AdaRedemptionStore from './AdaRedemptionStore';
import NodeUpdateStore from './NodeUpdateStore';
import AdaWalletSettingsStore from './AdaWalletSettingsStore';
import AddressesStore from './AddressesStore';

export const adaStoreClasses = {
  wallets: AdaWalletsStore,
  transactions: TransactionsStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: AdaWalletSettingsStore,
  addresses: AddressesStore,
};

export type AdaStoresMap = {
  wallets: AdaWalletsStore,
  transactions: TransactionsStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: AdaWalletSettingsStore,
  addresses: AddressesStore,
};

const adaStores = observable({
  wallets: null,
  transactions: null,
  adaRedemption: null,
  nodeUpdate: null,
  walletSettings: null,
  addresses: null
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
