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

export type TrackedDevice = {
  deviceModel: string;
  descriptor: string;
  device: Device;
};

export type DectorUnsubscriber = () => void;

export type Detector = (
  onAdd: (arg0: TrackedDevice) => void,
  onRemove: (arg0: TrackedDevice) => void
) => DectorUnsubscriber;
