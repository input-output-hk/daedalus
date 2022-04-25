import { ledgerUSBVendorId } from '@ledgerhq/devices';
import usbDetect from 'usb-detection';

import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { Detector } from './types';

const deviceToLog = ({ productId, locationId, deviceAddress }) =>
  `productId=${productId} locationId=${locationId} deviceAddress=${deviceAddress}`;

let isMonitoring = false;

const USB_EVENT_BUFFER_DELAY = 1500;

const monitorUSBDevices = () => {
  if (!isMonitoring) {
    isMonitoring = true;
    usbDetect.startMonitoring();
  }
};

const stopMonitoring = () => {
  if (isMonitoring) {
    // redeem the monitoring so the process can be terminated.
    usbDetect.stopMonitoring();
  }
};

// No better way for now. see https://github.com/LedgerHQ/ledgerjs/issues/434
process.on('exit', () => {
  stopMonitoring();
});

const addEvent = `add:${ledgerUSBVendorId}`;
const removeEvent = `remove:${ledgerUSBVendorId}`;

export const detectDevices: Detector = (onAdd, onRemove) => {
  let timeout;

  monitorUSBDevices();

  const deviceTracker = new DeviceTracker();

  const add = (device: usbDetect.Device) => {
    logger.info(
      `[HW-DEBUG] USB-DETECTION ADDED DEVICE: ${deviceToLog(device)}`
    );

    if (!timeout) {
      // a time is needed for the device to actually be connectable over HID..
      // we also take this time to not emit the device yet and potentially cancel it if a remove happens.
      timeout = setTimeout(() => {
        const newDevice = deviceTracker.findNewDevice();

        if (newDevice) {
          onAdd(newDevice);
        }

        timeout = null;
      }, USB_EVENT_BUFFER_DELAY);
    }
  };

  const remove = (device: usbDetect.Device) => {
    logger.info(
      `[HW-DEBUG] USB-DETECTION REMOVED DEVICE: ${deviceToLog(device)}`
    );

    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    } else {
      const removedDevice = deviceTracker.findRemovedDevice();

      if (removedDevice) {
        onRemove(removedDevice);
      }
    }
  };

  usbDetect.on(addEvent, add);
  usbDetect.on(removeEvent, remove);

  return () => {
    if (timeout) clearTimeout(timeout);
    // @ts-expect-error not all EventEmitter methods are covered in its definition file
    usbDetect.off(addEvent, add);
    // @ts-expect-error not all EventEmitter methods are covered in its definition file
    usbDetect.off(removeEvent, remove);
  };
};
