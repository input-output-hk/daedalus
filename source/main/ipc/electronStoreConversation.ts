import ElectronStore from 'electron-store';
import { ELECTRON_STORE_CHANNEL } from '../../common/ipc/api';
import { MainIpcConversation } from './lib/MainIpcConversation';
import { environment } from '../environment';
import {
  STORAGE_TYPES as types,
  STORAGE_KEYS as keys,
} from '../../common/config/electron-store.config';
import type { ElectronStoreMessage } from '../../common/ipc/api';
import type { StorageKey } from '../../common/types/electron-store.types';

const store = new ElectronStore();
// MainIpcChannel<Incoming, Outgoing>
export const electronStoreConversation: MainIpcConversation<
  ElectronStoreMessage,
  ElectronStoreMessage
> = new MainIpcConversation(ELECTRON_STORE_CHANNEL);

const getNetworkKey = (key: string) => `${environment.network}-${key}`;

const unset = async (key: StorageKey) =>
  requestElectronStore({
    type: types.DELETE,
    key,
  });

const reset = async () => {
  await Promise.all(Object.values(keys).map(unset));
};

export const requestElectronStore = (request: ElectronStoreMessage) => {
  const { type, key, data, id } = request;
  const keyWithId = id ? `${key}.${id}` : key;
  const networkKey = getNetworkKey(keyWithId);

  switch (type) {
    case types.GET:
      return store.get(networkKey);

    case types.DELETE:
      return store.delete(networkKey);

    case types.SET:
      return store.set(networkKey, data);

    case types.RESET:
      reset();
      return store.get(networkKey);

    default:
      return Promise.reject(new Error(`Invalid type ${type} provided.`));
  }
};
export const handleElectronStoreChannel = () => {
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(request: ElectronStoreMessage) ... Remove this comment to see the full error message
  electronStoreConversation.onRequest(requestElectronStore);
};
