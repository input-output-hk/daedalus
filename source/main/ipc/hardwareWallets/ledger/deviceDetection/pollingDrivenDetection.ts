import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { Detector } from './types';

export const detectDevices: Detector = (onAdd, onRemove) => {
  let timer;

  const stopPolling = () => {
    if (timer) {
      clearInterval(timer);
    }
  };

  process.on('exit', () => {
    stopPolling();
  });

  const deviceTracker = new DeviceTracker();

  const runPolling = () => {
    logger.info('[HW-DEBUG] Polling devices');
    const newDevice = deviceTracker.findNewDevice();

    if (newDevice) {
      onAdd(newDevice);
    }

    const removedDevice = deviceTracker.findRemovedDevice();

    if (removedDevice) {
      onRemove(removedDevice);
    }
  };

  timer = setInterval(runPolling, 1000);

  return () => {
    stopPolling();
  };
};
