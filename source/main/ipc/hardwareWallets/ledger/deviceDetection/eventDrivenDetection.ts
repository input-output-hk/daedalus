import { usb, WebUSB } from 'usb';
import { app } from 'electron';

import { logger } from '../../../../utils/logging';
import { DeviceTracker } from './deviceTracker';
import { Detector } from './types';

const USB_EVENT_BUFFER_DELAY = 1500;

const deviceToLog = ({ deviceAddress, busNumber, deviceDescriptor }) =>
  `productId=${deviceDescriptor.idProduct} busNumber=${busNumber} deviceAddress=${deviceAddress}`;

export const detectDevices: Detector = async (onAdd, onRemove) => {
  let timeout;
  // Start device monitoring
  const customWebUSB = new WebUSB({ allowAllDevices: true });
  // Device tracking (isNew/Exist...)
  const deviceTracker = new DeviceTracker();

  const add = (device: usbDetect.Device) => {
    logger.info(
      `[HW-DEBUG] NODE-USB ADDED DEVICE: ${deviceToLog(device)}`
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
      `[HW-DEBUG] NODE-USB REMOVED DEVICE: ${deviceToLog(device)}`
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
    logger.info('[HW-DEBUG] LISTENER OFF');
    if (timeout) clearTimeout(timeout);
    return customWebUSB;
  };
};
