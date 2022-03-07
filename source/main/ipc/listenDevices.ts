import usbDetect from 'usb-detection';
import { ledgerUSBVendorId, identifyUSBProductId } from '@ledgerhq/devices';
import { getDevices } from '@ledgerhq/hw-transport-node-hid-noevents';

import { log, listen } from '@ledgerhq/logs';
import debounce from 'lodash/debounce';

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

// listen(console.log);

const deviceToLog = ({ productId, locationId, deviceAddress }) =>
  `productId=${productId} locationId=${locationId} deviceAddress=${deviceAddress}`;

let usbDebounce = 100;
export const setUsbDebounce = (n: number) => {
  usbDebounce = n;
};
let monitoring = false;

const monitor = () => {
  if (!monitoring) {
    monitoring = true;
    usbDetect.startMonitoring();
  }

  return () => {};
};

let listDevices = getDevices();

const flatDevice = (d) => d.path;

const getFlatDevices = () => [
  ...new Set(getDevices().map((d) => flatDevice(d))),
];

const getDeviceByPaths = (paths) =>
  listDevices.find((d) => paths.includes(flatDevice(d)));

let lastDevices = getFlatDevices();

const getPayloadData = (type: 'add' | 'remove', device: Device) => {
  const descriptor: string = device.path;
  const deviceModel = identifyUSBProductId(device.productId);
  return { type, device, deviceModel, descriptor };
};

// No better way for now. see https://github.com/LedgerHQ/ledgerjs/issues/434
process.on('exit', () => {
  if (monitoring) {
    // redeem the monitoring so the process can be terminated.
    usbDetect.stopMonitoring();
  }
});

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
  monitor();
  const addEvent = `add:${ledgerUSBVendorId}`;
  const removeEvent = `remove:${ledgerUSBVendorId}`;

  Promise.resolve(getDevices()).then((devices) => {
    // this needs to run asynchronously so the subscription is defined during this phase
    for (const device of devices) {
      onAdd(getPayloadData('add', device));
    }
  });

  let timeout;

  const poll = () => {
    log('hid-listen', 'Polling for added or removed devices');

    let changeFound = false;
    const currentDevices = getFlatDevices();
    const newDevices = currentDevices.filter((d) => !lastDevices.includes(d));

    console.log('currentDevices', currentDevices);
    console.log('newDevices', newDevices);

    if (newDevices.length > 0) {
      log('hid-listen', 'New device found:', newDevices);

      listDevices = getDevices();
      console.log('listDevices', listDevices);
      console.log(
        'newDevices::getDeviceByPaths(newDevices)',
        getDeviceByPaths(newDevices)
      );
      onAdd(getPayloadData('add', getDeviceByPaths(newDevices)));

      changeFound = true;
    } else {
      log('hid-listen', 'No new device found');
    }

    const removeDevices = lastDevices.filter(
      (d) => !currentDevices.includes(d)
    );

    if (removeDevices.length > 0) {
      log('hid-listen', 'Removed device found:', removeDevices);
      console.log('removeDevices', removeDevices);
      console.log(
        'removeDevices::getDeviceByPaths(removeDevices)',
        getDeviceByPaths(removeDevices)
      );

      onRemove(getPayloadData('remove', getDeviceByPaths(removeDevices)));
      listDevices = listDevices.filter(
        (d) => !removeDevices.includes(flatDevice(d))
      );

      changeFound = true;
    } else {
      log('hid-listen', 'No removed device found');
    }

    if (changeFound) {
      lastDevices = currentDevices;
    }
  };

  const debouncedPoll = debounce(poll, usbDebounce);

  const add = (device) => {
    log('usb-detection', `add: ${deviceToLog(device)}`);

    if (!timeout) {
      // a time is needed for the device to actually be connectable over HID..
      // we also take this time to not emit the device yet and potentially cancel it if a remove happens.
      timeout = setTimeout(() => {
        debouncedPoll();
        timeout = null;
      }, usbDebounce);
    }
  };

  const remove = (device) => {
    console.log('REMOVE ******************');
    log('usb-detection', `remove: ${deviceToLog(device)}`);

    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    } else {
      debouncedPoll();
    }
  };

  usbDetect.on(addEvent, add);
  usbDetect.on(removeEvent, remove);

  // setInterval(poll, 1000);
};
