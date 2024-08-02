import TrezorConnect from '@trezor/connect';
import { logger } from '../utils/logging';
import { manifest } from './manifest';

export const initTrezorConnect = async () => {
  console.debug('>>> INIT TREZOR');
  try {
    await TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: process.env.DEBUG_TREZOR === 'true',
      manifest,
    });
    console.debug('>>> INIT TREZOR - done');
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.init()');
  } catch (error) {
    console.debug('>>> INIT TREZOR - error: ', error);
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.init()');
  }
};

export const reinitTrezorConnect = async () => {
  console.debug('>>> Re-INIT TREZOR');
  try {
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.dispose()');
    await TrezorConnect.dispose();
    console.debug('>>> re-INIT TREZOR - done');
  } catch (error) {
    console.debug('>>> re-INIT TREZOR - error: ', error);
    // ignore any TrezorConnect instance disposal errors
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.dispose()');
  }

  return initTrezorConnect();
};
