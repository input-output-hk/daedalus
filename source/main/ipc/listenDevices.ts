import { identifyUSBProductId, ledgerUSBVendorId } from '@ledgerhq/devices';
import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';

import usbDetect from 'usb-detection';

import { log, listen } from '@ledgerhq/logs';
import debounce from 'lodash/debounce';
import { logger } from '../utils/logging';

export type Device = {
  vendorId: number;
  productId: number;
  path: string;
  deviceName: string;
  manufacturer: string;
  serialNumber: string;
  deviceAddress: number;
  product: string;
  release: number;
  interface: number;
  usagePage: number;
  usage: number;
};

listen(logger.info);

let isMonitoring = false;

const deviceToLog = ({ productId, locationId, deviceAddress }) =>
  `productId=${productId} locationId=${locationId} deviceAddress=${deviceAddress}`;

const monitor = () => {
  if (!isMonitoring) {
    isMonitoring = true;
    usbDetect.startMonitoring();
  }
};

let usbDebounce = 100;
export const setUsbDebounce = (n: number) => {
  usbDebounce = n;
};

let deviceList = getDevices();

const getDevicePath = (d: Device) => d.path;

const getFlatDevices = () => [
  ...new Set(getDevices().map((d: Device) => getDevicePath(d))),
];

const getDeviceByPaths = (paths) =>
  deviceList.find((d: Device) => paths.includes(getDevicePath(d)));

const lastDevices = new Map();

const addLastDevice = (newDevices: Device[]) =>
  newDevices.forEach((d) => lastDevices.set(d.path, d));
addLastDevice(deviceList);

const getPayloadData = (type: 'add' | 'remove', device: Device) => {
  const descriptor: string = device.path;
  const deviceModel = identifyUSBProductId(device.productId);
  return { type, device, deviceModel, descriptor };
};

// No better way for now. see https://github.com/LedgerHQ/ledgerjs/issues/434
process.on('exit', () => {
  if (isMonitoring) {
    // redeem the monitoring so the process can be terminated.
    usbDetect.stopMonitoring();
  }

  if (timer) {
    clearInterval(timer);
  }
});

let timer;

type Payload = {
  type: 'add' | 'remove';
  device: Device;
  deviceModel: string;
  descriptor: string;
};

export const listenDevices = (
  onAdd: (arg0: Payload) => void,
  onRemove: (arg0: Payload) => void
) => {
  const addEvent = `add:${ledgerUSBVendorId}`;
  const removeEvent = `remove:${ledgerUSBVendorId}`;
  let timeout;

  monitor();

  Promise.resolve(getDevices()).then((devices) => {
    // this needs to run asynchronously so the subscription is defined during this phase
    for (const device of devices) {
      onAdd(getPayloadData('add', device));
    }
  });

  const poll = () => {
    log('[HID-LISTEN]', 'Polling for added or removed devices');

    const currentDevices = getFlatDevices();
    const newDevices = currentDevices.filter((d) => !lastDevices.has(d));

    if (newDevices.length > 0) {
      log('[HID-LISTEN]', 'New device found:', {
        newDevices,
        currentDevices,
        lastDevices: Array.from(lastDevices.keys()),
      });

      deviceList = getDevices();
      onAdd(getPayloadData('add', getDeviceByPaths(newDevices)));
      addLastDevice(deviceList);
    } else {
      log('[HID-LISTEN]', 'No new device found', {
        currentDevices,
        lastDevices: Array.from(lastDevices.keys()),
      });
    }

    const removeDevices = Array.from(lastDevices.keys())
      .filter((d) => !currentDevices.includes(d))
      .map((d) => d);

    if (removeDevices.length > 0) {
      const key = removeDevices[0];
      const removedDevice = lastDevices.get(key);
      log('[HID-LISTEN]', 'Removed device found:', {
        removeDevices,
        devices: removedDevice,
        currentDevices,
        lastDevices: Array.from(lastDevices.keys()),
      });

      onRemove(getPayloadData('remove', removedDevice));

      lastDevices.delete(key);
    } else {
      log('[HID-LISTEN]', 'No removed device found', {
        currentDevices,
        lastDevices: Array.from(lastDevices.keys()),
      });
    }
  };

  const debouncedPoll = debounce(poll, usbDebounce);

  const add = (device: usbDetect.Device) => {
    log('[USB-DETECTION]', `add: ${deviceToLog(device)}`);

    if (!timeout) {
      // a time is needed for the device to actually be connectable over HID..
      // we also take this time to not emit the device yet and potentially cancel it if a remove happens.
      timeout = setTimeout(() => {
        debouncedPoll();
        timeout = null;
      }, usbDebounce);
    }
  };

  const remove = (device: usbDetect.Device) => {
    log('[USB-DETECTION]', `remove: ${deviceToLog(device)}`);

    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    } else {
      debouncedPoll();
    }
  };

  if (TransportNodeHid.isSupported()) {
    logger.info('[LISTEN-LEDGER-DEVICES] Using usb-detection');
    usbDetect.on(addEvent, add);
    usbDetect.on(removeEvent, remove);
  } else {
    logger.info('[LISTEN-LEDGER-DEVICES] Using polling');
    timer = setInterval(debouncedPoll, 1000);
  }
};
