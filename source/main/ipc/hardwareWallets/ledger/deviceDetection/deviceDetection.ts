import TransportNodeHid from '@ledgerhq/hw-transport-node-hid-noevents';

import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { detectDevices as useEventDrivenDetection } from './eventDrivenDetection';
import { detectDevices as usePollingDrivenDetection } from './pollingDrivenDetection';
import { Detector, TrackedDevice, DectorUnsubscriber } from './types';

export type DeviceDetectionPayload = {
  type: 'add' | 'remove';
} & TrackedDevice;

export const deviceDetection = (
  onAdd: (arg0: DeviceDetectionPayload) => void,
  onRemove: (arg0: DeviceDetectionPayload) => void
) => {
  Promise.resolve(DeviceTracker.getDevices()).then((devices) => {
    // this needs to run asynchronously so the subscription is defined during this phase
    for (const device of devices) {
      onAdd({
        type: 'add',
        ...DeviceTracker.getTrackedDeviceByPath(device.path),
      });
    }
  });

  const handleOnAdd = (trackedDevice: TrackedDevice) =>
    onAdd({ type: 'add', ...trackedDevice });
  const handleOnRemove = (trackedDevice: TrackedDevice) =>
    onRemove({ type: 'remove', ...trackedDevice });

  let detectDevices: Detector;

  if (TransportNodeHid.isSupported()) {
    logger.info('[HW-DEBUG] Using usb-detection');

    detectDevices = useEventDrivenDetection;
  } else {
    logger.info('[HW-DEBUG] Using polling');

    detectDevices = usePollingDrivenDetection;
  }

  detectDevices(handleOnAdd, handleOnRemove);
};

export const waitForDevice = () => {
  return new Promise<TrackedDevice>(async (resolve) => {
    const currentDevices = await DeviceTracker.getDevices();

    for (const device of currentDevices) {
      return resolve(DeviceTracker.getTrackedDeviceByPath(device.path));
    }

    let detectDevices: Detector;
    let unsubscribe: DectorUnsubscriber = null;

    if (TransportNodeHid.isSupported()) {
      logger.info('[HW-DEBUG] Using usb-detection');

      detectDevices = useEventDrivenDetection;
    } else {
      logger.info('[HW-DEBUG] Using polling');

      detectDevices = usePollingDrivenDetection;
    }

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
