'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.detectDevices = void 0;
const devices_1 = require('@ledgerhq/devices');
const usb_detection_1 = __importDefault(require('usb-detection'));
const logging_1 = require('../../../../utils/logging');
const deviceTracker_1 = require('./deviceTracker');
const deviceToLog = ({ productId, locationId, deviceAddress }) =>
  `productId=${productId} locationId=${locationId} deviceAddress=${deviceAddress}`;
let isMonitoring = false;
const USB_EVENT_BUFFER_DELAY = 1500;
const monitorUSBDevices = () => {
  if (!isMonitoring) {
    isMonitoring = true;
    usb_detection_1.default.startMonitoring();
  }
};
const stopMonitoring = () => {
  if (isMonitoring) {
    // redeem the monitoring so the process can be terminated.
    usb_detection_1.default.stopMonitoring();
  }
};
// No better way for now. see https://github.com/LedgerHQ/ledgerjs/issues/434
process.on('exit', () => {
  stopMonitoring();
});
const addEvent = `add:${devices_1.ledgerUSBVendorId}`;
const removeEvent = `remove:${devices_1.ledgerUSBVendorId}`;
const detectDevices = (onAdd, onRemove) => {
  let timeout;
  monitorUSBDevices();
  const deviceTracker = new deviceTracker_1.DeviceTracker();
  const add = (device) => {
    logging_1.logger.info(
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
  const remove = (device) => {
    logging_1.logger.info(
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
  usb_detection_1.default.on(addEvent, add);
  usb_detection_1.default.on(removeEvent, remove);
  return () => {
    if (timeout) clearTimeout(timeout);
    // @ts-expect-error not all EventEmitter methods are covered in its definition file
    usb_detection_1.default.off(addEvent, add);
    // @ts-expect-error not all EventEmitter methods are covered in its definition file
    usb_detection_1.default.off(removeEvent, remove);
  };
};
exports.detectDevices = detectDevices;
//# sourceMappingURL=eventDrivenDetection.js.map
