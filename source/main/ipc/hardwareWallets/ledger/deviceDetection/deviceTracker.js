'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.DeviceTracker = void 0;
const hw_transport_node_hid_noevents_1 = require('@ledgerhq/hw-transport-node-hid-noevents');
const devices_1 = require('@ledgerhq/devices');
const logging_1 = require('../../../../utils/logging');
class DeviceTracker {
  knownDevices;
  static getUniqueDevices() {
    return [
      ...new Set(
        (0, hw_transport_node_hid_noevents_1.getDevices)().map((d) => d.path)
      ),
    ];
  }
  static getDeviceByPath(path) {
    return (0, hw_transport_node_hid_noevents_1.getDevices)().find(
      (d) => d.path === path
    );
  }
  static getTrackedDeviceByPath(path) {
    const device = DeviceTracker.getDeviceByPath(path);
    const descriptor = device.path;
    const deviceModel = (0, devices_1.identifyUSBProductId)(device.productId);
    return { device, deviceModel, descriptor };
  }
  static getDevices() {
    return (0, hw_transport_node_hid_noevents_1.getDevices)();
  }
  constructor() {
    this.knownDevices = new Map();
    (0, hw_transport_node_hid_noevents_1.getDevices)()?.forEach((d) =>
      this.knownDevices.set(d.path, d)
    );
  }
  findNewDevice() {
    const currentDevices = DeviceTracker.getUniqueDevices();
    const [newDevicePath] = currentDevices.filter(
      (d) => !this.knownDevices.has(d)
    );
    const knownDevicesPath = Array.from(this.knownDevices.keys());
    if (newDevicePath) {
      const newDevice = DeviceTracker.getTrackedDeviceByPath(newDevicePath);
      this.knownDevices.set(newDevicePath, newDevice);
      logging_1.logger.info('[HW-DEBUG] DeviceTracker - New device found:', {
        newDevicePath,
        currentDevices,
        knownDevicesPath,
      });
      return newDevice;
    }
    logging_1.logger.info('[HW-DEBUG] DeviceTracker - No new device found:', {
      currentDevices,
      knownDevicesPath,
    });
    return null;
  }
  findRemovedDevice() {
    const currentDevices = DeviceTracker.getUniqueDevices();
    const [removedDevicePath] = Array.from(this.knownDevices.keys())
      .filter((d) => !currentDevices.includes(d))
      .map((d) => d);
    const knownDevicesPath = Array.from(this.knownDevices.keys());
    logging_1.logger.info('[HW-DEBUG] DeviceTracker - Removed device path', {
      removedDevicePath,
    });
    if (removedDevicePath) {
      const removedDevice = this.knownDevices.get(removedDevicePath);
      this.knownDevices.delete(removedDevicePath);
      logging_1.logger.info(
        '[HW-DEBUG] DeviceTracker - Removed device found:',
        {
          removedDevicePath,
          removedDevice,
          currentDevices,
          knownDevicesPath,
        }
      );
      return removedDevice;
    }
    logging_1.logger.info(
      '[HW-DEBUG] DeviceTracker - No removed device found:',
      {
        currentDevices,
        knownDevicesPath,
      }
    );
    return null;
  }
}
exports.DeviceTracker = DeviceTracker;
//# sourceMappingURL=deviceTracker.js.map
