'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.reinitTrezorConnect = exports.initTrezorConnect = void 0;
const connect_1 = __importDefault(require('@trezor/connect'));
const logging_1 = require('../utils/logging');
const manifest_1 = require('./manifest');
const initTrezorConnect = async () => {
  connect_1.default
    .init({
      popup: false,
      debug: false,
      // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
      // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
      // this is useful when you don't know if you are dealing with Trezor user
      // see what's going on inside connect
      manifest: manifest_1.manifest,
      transports: ['NodeUsbTransport'],
    })
    .then(() => {
      logging_1.logger.info('[HW-DEBUG] TrezorConnect is ready!');
    })
    .catch((error) => {
      logging_1.logger.error(`[HW-DEBUG] TrezorConnect init error:`, error);
    });
};
exports.initTrezorConnect = initTrezorConnect;
const reinitTrezorConnect = async () => {
  try {
    logging_1.logger.info('[TREZOR-CONNECT] Called TrezorConnect.dispose()');
    await connect_1.default.dispose();
  } catch (error) {
    // ignore any TrezorConnect instance disposal errors
    logging_1.logger.error(
      '[TREZOR-CONNECT] Failed to call TrezorConnect.dispose()',
      error
    );
  }
  return (0, exports.initTrezorConnect)();
};
exports.reinitTrezorConnect = reinitTrezorConnect;
//# sourceMappingURL=connection.js.map
