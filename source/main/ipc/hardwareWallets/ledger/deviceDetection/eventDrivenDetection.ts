import { usb, Device } from 'usb';
import { ledgerUSBVendorId } from '@ledgerhq/devices';

import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { Detector } from './types';

const USB_EVENT_BUFFER_DELAY = 1500;

const isLedgerDevice = (device: Device) =>
  device.deviceDescriptor.idVendor === ledgerUSBVendorId;

const deviceToLog = (device: Device) =>
  `productId=${device.deviceDescriptor.idProduct} busNumber=${device.busNumber} deviceAddress=${device.deviceAddress}`;

export const detectDevices: Detector = (onAdd, onRemove) => {
  let timeout: ReturnType<typeof setTimeout> | null = null;

  const deviceTracker = new DeviceTracker();

  const add = (device: Device) => {
    if (!isLedgerDevice(device)) return;

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

  const remove = (device: Device) => {
    if (!isLedgerDevice(device)) return;

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

  usb.on('attach', add);
  usb.on('detach', remove);

  return () => {
    if (timeout) clearTimeout(timeout);
    usb.off('attach', add);
    usb.off('detach', remove);
  };
};
