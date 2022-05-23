import TrezorConnect from 'trezor-connect';
import { logger } from '../utils/logging';
import { manifest } from './manifest';

let alreadyInited = false;

export const initTrezorConnect = async () => {
  try {
    // TODO - we need to find another way to reinitialize Trezor
    // previously it was possible to re-init Trezor by calling TrezorConnect.init() multiple times and therefore reset the passphrase cached on device
    if (alreadyInited) {
      return;
    }

    await TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: true,
      manifest,
    });
    alreadyInited = true;

    logger.info('[TREZOR-CONNECT] Called TrezorConnect.init()');
  } catch (error) {
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.init()');
    throw error;
  }
};

export const reinitTrezorConnect = initTrezorConnect;
