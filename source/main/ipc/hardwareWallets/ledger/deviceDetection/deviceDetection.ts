import TransportNodeHid from '@ledgerhq/hw-transport-node-hid-noevents';

import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { detectDevices as useEventDrivenDetection } from './eventDrivenDetection';
import { detectDevices as usePollingDrivenDetection } from './pollingDrivenDetection';
import { Detector, TrackedDevice, DectorUnsubscriber } from './types';

export type DeviceDetectionPayload = {
  type: 'add' | 'remove';
} & TrackedDevice;

const getDetector = () => {
  if (TransportNodeHid.isSupported()) {
    logger.info('[HW-DEBUG] Using usb-detection');

    return useEventDrivenDetection;
  }
  logger.info('[HW-DEBUG] Using polling');

  return usePollingDrivenDetection;
};

export const deviceDetection = (
  onAdd: (payload: DeviceDetectionPayload) => void,
  onRemove: (payload: DeviceDetectionPayload) => void
) => {
  // detect existing connected devices without blocking the subscription registration
  // https://github.com/LedgerHQ/ledgerjs/blob/master/packages/hw-transport-node-hid-singleton/src/TransportNodeHid.ts#L56
  Promise.resolve(DeviceTracker.getDevices())
    .then((devices) => {
      // this needs to run asynchronously so the subscription is defined during this phase
      for (const device of devices) {
        onAdd({
          type: 'add',
          ...DeviceTracker.getTrackedDeviceByPath(device.path),
        });
      }
    })
    .catch((e) => {
      logger.error(`[HW-DEBUG] ERROR getting devices ${JSON.stringify(e)}`);
    });

  const handleOnAdd = (trackedDevice: TrackedDevice) =>
    onAdd({ type: 'add', ...trackedDevice });
  const handleOnRemove = (trackedDevice: TrackedDevice) =>
    onRemove({ type: 'remove', ...trackedDevice });

  const detectDevices = getDetector();

  detectDevices(handleOnAdd, handleOnRemove);
};

export const waitForDevice = () => {
  logger.debug('[HW-DEBUG] Wait for device ...');
  return new Promise<TrackedDevice>((resolve) => {
    const currentDevices = DeviceTracker.getDevices();

    if (currentDevices.length > 0) {
      resolve(DeviceTracker.getTrackedDeviceByPath(currentDevices[0].path));
      return;
    }

    const detectDevices = getDetector();
    let unsubscribe: DectorUnsubscriber = null;

    const handleOnAdd = (trackedDevice: TrackedDevice) => {
      if (unsubscribe) {
        unsubscribe();
      }

      return resolve(trackedDevice);
    };

    const handleOnRemove = () => false;

    unsubscribe = detectDevices(handleOnAdd, handleOnRemove);
  });
};
