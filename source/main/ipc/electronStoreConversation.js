'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleElectronStoreChannel = exports.requestElectronStore = exports.electronStoreConversation = void 0;
const electron_store_1 = __importDefault(require('electron-store'));
const api_1 = require('../../common/ipc/api');
const MainIpcConversation_1 = require('./lib/MainIpcConversation');
const environment_1 = require('../environment');
const electron_store_config_1 = require('../../common/config/electron-store.config');
const store = new electron_store_1.default();
// MainIpcChannel<Incoming, Outgoing>
exports.electronStoreConversation = new MainIpcConversation_1.MainIpcConversation(
  api_1.ELECTRON_STORE_CHANNEL
);
const getNetworkKey = (key) => `${environment_1.environment.network}-${key}`;
const unset = async (key) =>
  (0, exports.requestElectronStore)({
    type: electron_store_config_1.STORAGE_TYPES.DELETE,
    key,
  });
const reset = async () => {
  await Promise.all(
    Object.values(electron_store_config_1.STORAGE_KEYS).map(unset)
  );
};
const requestElectronStore = (request) => {
  const { type, key, data, id } = request;
  const keyWithId = id ? `${key}.${id}` : key;
  const networkKey = getNetworkKey(keyWithId);
  switch (type) {
    case electron_store_config_1.STORAGE_TYPES.GET:
      return store.get(networkKey);
    case electron_store_config_1.STORAGE_TYPES.DELETE:
      return store.delete(networkKey);
    case electron_store_config_1.STORAGE_TYPES.SET:
      return store.set(networkKey, data);
    case electron_store_config_1.STORAGE_TYPES.RESET:
      reset();
      return store.get(networkKey);
    default:
      return Promise.reject(new Error(`Invalid type ${type} provided.`));
  }
};
exports.requestElectronStore = requestElectronStore;
const handleElectronStoreChannel = () => {
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '(request: ElectronStoreMessage) ... Remove this comment to see the full error message
  exports.electronStoreConversation.onRequest(exports.requestElectronStore);
};
exports.handleElectronStoreChannel = handleElectronStoreChannel;
//# sourceMappingURL=electronStoreConversation.js.map
