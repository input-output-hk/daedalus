// @flow
import ElectronStore from 'electron-store';
import type { ElectronStoreMessage } from '../../common/ipc/api';
import { ELECTRON_STORE_CHANNEL } from '../../common/ipc/api';
import { MainIpcConversation } from './lib/MainIpcConversation';

const store = new ElectronStore();

// MainIpcChannel<Incoming, Outgoing>
export const electronStoreConversation: MainIpcConversation<
  ElectronStoreMessage,
  any
> = new MainIpcConversation(ELECTRON_STORE_CHANNEL);

export const handleElectronStoreChannel = () => {
  electronStoreConversation.onRequest(request => {
    const { type, key, data } = request;
    switch (type) {
      case 'get':
        return store.get(key);
      case 'delete':
        return store.delete(key);
      case 'set':
        return store.set(key, data);
      default:
        return Promise.reject(new Error(`Invalid type ${type} provided.`));
    }
  });
};
