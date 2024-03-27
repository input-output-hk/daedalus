'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.storeRtsFlagsSettings = exports.getRtsFlagsSettings = void 0;
const electron_store_1 = __importDefault(require('electron-store'));
const logging_1 = require('./logging');
const store = new electron_store_1.default();
const getStoreKey = (network) => `${network}-RTS-FLAGS`;
const getRtsFlagsSettings = (network) => {
  try {
    const flags = store.get(getStoreKey(network));
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info(
      `[RTS-FLAGS] Read ${network} flags: ${flags} from config`
    );
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    return flags;
  } catch (error) {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.error(
      `[RTS-FLAGS] Failed to read ${network} flags from config`,
      error
    );
  }
  return null;
};
exports.getRtsFlagsSettings = getRtsFlagsSettings;
const storeRtsFlagsSettings = (network, flags) => {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  logging_1.logger.info(
    `[RTS-FLAGS] Persisted ${network} flags: [${flags.toString()}] in config`
  );
  store.set(getStoreKey(network), flags);
};
exports.storeRtsFlagsSettings = storeRtsFlagsSettings;
//# sourceMappingURL=rtsFlagsSettings.js.map
