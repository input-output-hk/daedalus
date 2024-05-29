import TrezorConnect from '@trezor/connect';
import { logger } from '../utils/logging';
import { manifest } from './manifest';

export const initTrezorConnect = async () => {
  TrezorConnect.init({
    popup: false, // render your own UI
    debug: false, // see what's going on inside connect
    // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
    // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
    // this is useful when you don't know if you are dealing with Trezor user
    manifest,
    transports: ['BridgeTransport'],
  })
    .then(() => {
      logger.info('[HW-DEBUG] TrezorConnect is ready!');
    })
    .catch((error) => {
      logger.error(`[HW-DEBUG] TrezorConnect init error:`, error);
    });
};

export const reinitTrezorConnect = async () => {
  try {
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.dispose()');
    await TrezorConnect.dispose();
  } catch (error) {
    // ignore any TrezorConnect instance disposal errors
    logger.error(
      '[TREZOR-CONNECT] Failed to call TrezorConnect.dispose()',
      error
    );
  }

  return initTrezorConnect();
};
