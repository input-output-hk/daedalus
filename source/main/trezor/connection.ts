import TrezorConnect from 'trezor-connect';
import { logger } from '../utils/logging';

export const initTrezorConnect = async () => {
  try {
    await TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: true,
      manifest: {
        email: 'support@iohk.io',
        appUrl: 'https://daedaluswallet.io/',
      },
    });
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.init()');
  } catch (error) {
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.init()');
    throw error;
  }
};

export const reinitTrezorConnect = initTrezorConnect;
