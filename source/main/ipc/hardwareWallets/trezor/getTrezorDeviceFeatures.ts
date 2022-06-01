import TrezorConnect from 'trezor-connect';
import { logger } from '../../../utils/logging';
import { wait } from '../../../../common/utils/wait';

export const getTrezorDeviceFeatures = async (retries = 3) => {
  const features = await TrezorConnect.getFeatures();

  if (retries < 0) {
    throw new Error('Failed to get Trezor device features');
  }

  if (features.success === false) {
    if (
      features.payload.code === 'Device_CallInProgress' ||
      features.payload.error === 'wrong previous session' ||
      features.payload.error === 'device disconnected during action'
    ) {
      logger.info(
        '[TREZOR-CONNECT] Got Device_CallInProgress error - retrying'
      );

      await wait(1000);
      return getTrezorDeviceFeatures(retries - 1);
    }

    logger.error(
      '[TREZOR-CONNECT] Got other error - not retrying',
      features.payload
    );

    throw features.payload;
  }

  return features;
};
