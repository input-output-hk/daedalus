// @flow
import { observable, action } from 'mobx';
import WalletsStore from './WalletsStore';
import TransactionsStore from './TransactionsStore';
import WalletBackupStore from './WalletBackupStore';
import NetworkStatusStore from '../NetworkStatusStore';
import AdaRedemptionStore from './AdaRedemptionStore';
import NodeUpdateStore from './NodeUpdateStore';
import WalletSettingsStore from './WalletSettingsStore';
import AddressesStore from './AddressesStore';

export const adaStoreClasses = {
  wallets: WalletsStore,
  transactions: TransactionsStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: WalletSettingsStore,
  addresses: AddressesStore,
};

export type AdaStoresMap = {
  wallets: WalletsStore,
  transactions: TransactionsStore,
  walletBackup: WalletBackupStore,
  networkStatus: NetworkStatusStore,
  adaRedemption: AdaRedemptionStore,
  nodeUpdate: NodeUpdateStore,
  walletSettings: WalletSettingsStore,
  addresses: AddressesStore,
};

const adaStores = observable({
  wallets: null,
  transactions: null,
  walletBackup: null,
  networkStatus: null,
  adaRedemption: null,
  nodeUpdate: null,
  walletSettings: null,
  addresses: null
});

// Set up and return the stores and reset all stores to defaults
export default action((stores, api, actions): AdaStoresMap => {
  const storeNames = Object.keys(adaStoreClasses);
  storeNames.forEach(name => { if (adaStores[name]) adaStores[name].teardown(); });
  storeNames.forEach(name => { adaStores[name] = new adaStoreClasses[name](stores, api, actions); });
  storeNames.forEach(name => { if (adaStores[name]) adaStores[name].initialize(); });
  return adaStores;
});
