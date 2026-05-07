import { getDevices } from '@ledgerhq/hw-transport-node-hid-noevents';
import { identifyUSBProductId } from '@ledgerhq/devices';

import { logger } from '../../../../utils/logging';
import { Device, TrackedDevice, DeviceModel } from './types';

export class DeviceTracker {
  knownDevices: Map<string, TrackedDevice>;

  static getUniqueDevices() {
    return [...new Set<string>(getDevices().map((d: Device) => d.path))];
  }

  static getDeviceByPath(path: string): Device {
    return getDevices().find((d: Device) => d.path === path) as Device;
  }

  static getTrackedDeviceByPath(path: string) {
    const device = DeviceTracker.getDeviceByPath(path);

    const descriptor: string = device.path;
    const deviceModel = identifyUSBProductId(
      device.productId
    ) as unknown as DeviceModel;

    return { device, deviceModel, descriptor } as TrackedDevice;
  }

  static getDevices(): (Device & { deviceName?: string })[] {
    // Cast needed: node-hid Device.path is optional but we only use devices with paths
    return getDevices() as unknown as (Device & { deviceName?: string })[];
  }

  constructor() {
    this.knownDevices = new Map();

    // Cast needed: constructor tracks known paths; getTrackedDeviceByPath populates full TrackedDevice on first use
    getDevices()?.forEach(
      (d) =>
        d.path && this.knownDevices.set(d.path, d as unknown as TrackedDevice)
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

      logger.info('[HW-DEBUG] DeviceTracker - New device found:', {
        newDevicePath,
        currentDevices,
        knownDevicesPath,
      });

      return newDevice;
    }

    logger.info('[HW-DEBUG] DeviceTracker - No new device found:', {
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

    logger.info('[HW-DEBUG] DeviceTracker - Removed device path', {
      removedDevicePath,
    });

    if (removedDevicePath) {
      const removedDevice = this.knownDevices.get(removedDevicePath);
      this.knownDevices.delete(removedDevicePath);

      logger.info('[HW-DEBUG] DeviceTracker - Removed device found:', {
        removedDevicePath,
        removedDevice,
        currentDevices,
        knownDevicesPath,
      });

      return removedDevice;
    }

    logger.info('[HW-DEBUG] DeviceTracker - No removed device found:', {
      currentDevices,
      knownDevicesPath,
    });

    return null;
  }
}
