// @flow
import ElectronStore from 'electron-store';
import { ELECTRON_STORE_CHANNEL } from '../../common/ipc/api';
import { MainIpcConversation } from './lib/MainIpcConversation';
import { environment } from '../environment.js';
import type { ElectronStoreMessage } from '../../common/ipc/api';

const store = new ElectronStore();

// MainIpcChannel<Incoming, Outgoing>
export const electronStoreConversation: MainIpcConversation<
  ElectronStoreMessage,
  any
> = new MainIpcConversation(ELECTRON_STORE_CHANNEL);

const getNetworkKey = (key: string) => `${environment.network}-${key}`;

export const requestElectronStore = (request: ElectronStoreMessage) => {
  const { type, key, data, id } = request;
  const keyWithId = id ? `${key}.${id}` : key;
  const networkKey = getNetworkKey(keyWithId);
  switch (type) {
    case 'get':
      return store.get(networkKey);
    case 'delete':
      return store.delete(networkKey);
    case 'set':
      return store.set(networkKey, data);
    default:
      return Promise.reject(new Error(`Invalid type ${type} provided.`));
  }
};

export const handleElectronStoreChannel = () => {
  electronStoreConversation.onRequest(requestElectronStore);
};
