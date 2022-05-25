import TrezorConnect from 'trezor-connect';
import { logger } from '../utils/logging';
import { manifest } from './manifest';

export const initTrezorConnect = async () => {
  try {
    await TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: process.env.DEBUG_TREZOR === 'true',
      manifest,
    });

    logger.info('[TREZOR-CONNECT] Called TrezorConnect.init()');
  } catch (error) {
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.init()');
    throw error;
  }
};

export const reinitTrezorConnect = () => {
  try {
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.dispose()');
    TrezorConnect.dispose();
  } catch (error) {
    // ignore any TrezorConnect instance disposal errors
    logger.info('[TREZOR-CONNECT] Failed to call TrezorConnect.dispose()');
  }

  return initTrezorConnect();
};
