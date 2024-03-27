'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.detectDevices = void 0;
const logging_1 = require('../../../../utils/logging');
const deviceTracker_1 = require('./deviceTracker');
const detectDevices = (onAdd, onRemove) => {
  let timer;
  const stopPolling = () => {
    if (timer) {
      clearInterval(timer);
    }
  };
  process.on('exit', () => {
    stopPolling();
  });
  const deviceTracker = new deviceTracker_1.DeviceTracker();
  const runPolling = () => {
    logging_1.logger.info('[HW-DEBUG] Polling devices');
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
exports.detectDevices = detectDevices;
//# sourceMappingURL=pollingDrivenDetection.js.map
