import { CommonParams } from 'trezor-connect';
import { logger } from './logging';

export const buildTrezorDeviceParams = (
  path: string | null
): CommonParams['device'] => {
  if (path !== null) {
    logger.info(
      `[HW-DEBUG] Connecting Trezor device with ${path} as device path`
    );

    return {
      path,
    };
  }

  logger.info('[HW-DEBUG] Connecting Trezor device with null as device path');
  return undefined;
};
