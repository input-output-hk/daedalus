'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.waitForDevice = exports.deviceDetection = void 0;
const hw_transport_node_hid_noevents_1 = __importDefault(
  require('@ledgerhq/hw-transport-node-hid-noevents')
);
const logging_1 = require('../../../../utils/logging');
const deviceTracker_1 = require('./deviceTracker');
const eventDrivenDetection_1 = require('./eventDrivenDetection');
const pollingDrivenDetection_1 = require('./pollingDrivenDetection');
const getDetector = () => {
  if (hw_transport_node_hid_noevents_1.default.isSupported()) {
    logging_1.logger.info('[HW-DEBUG] Using usb-detection');
    return eventDrivenDetection_1.detectDevices;
  }
  logging_1.logger.info('[HW-DEBUG] Using polling');
  return pollingDrivenDetection_1.detectDevices;
};
const deviceDetection = (onAdd, onRemove) => {
  // detect existing connected devices without blocking the subscription registration
  // https://github.com/LedgerHQ/ledgerjs/blob/master/packages/hw-transport-node-hid-singleton/src/TransportNodeHid.ts#L56
  Promise.resolve(deviceTracker_1.DeviceTracker.getDevices())
    .then((devices) => {
      // this needs to run asynchronously so the subscription is defined during this phase
      for (const device of devices) {
        onAdd({
          type: 'add',
          ...deviceTracker_1.DeviceTracker.getTrackedDeviceByPath(device.path),
        });
      }
    })
    .catch((e) => {
      logging_1.logger.error(
        `[HW-DEBUG] ERROR getting devices ${JSON.stringify(e)}`
      );
    });
  const handleOnAdd = (trackedDevice) =>
    onAdd({ type: 'add', ...trackedDevice });
  const handleOnRemove = (trackedDevice) =>
    onRemove({ type: 'remove', ...trackedDevice });
  const detectDevices = getDetector();
  detectDevices(handleOnAdd, handleOnRemove);
};
exports.deviceDetection = deviceDetection;
const waitForDevice = () => {
  logging_1.logger.debug('[HW-DEBUG] Wait for device ...');
  return new Promise((resolve) => {
    const currentDevices = deviceTracker_1.DeviceTracker.getDevices();
    if (currentDevices.length > 0) {
      resolve(
        deviceTracker_1.DeviceTracker.getTrackedDeviceByPath(
          currentDevices[0].path
        )
      );
      return;
    }
    const detectDevices = getDetector();
    let unsubscribe = null;
    const handleOnAdd = (trackedDevice) => {
      if (unsubscribe) {
        unsubscribe();
      }
      return resolve(trackedDevice);
    };
    const handleOnRemove = () => false;
    unsubscribe = detectDevices(handleOnAdd, handleOnRemove);
  });
};
exports.waitForDevice = waitForDevice;
//# sourceMappingURL=deviceDetection.js.map
