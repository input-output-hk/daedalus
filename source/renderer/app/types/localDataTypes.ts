import { StorageKey } from '../../../common/types/electron-store.types';
import {
  DeviceType,
  HardwareWalletExtendedPublicKeyResponse,
  TransportDevice,
} from '../../../common/types/hardware-wallets.types';

export type WalletLocalData = {
  id: string;
  recoveryPhraseVerificationDate?: Date | null | undefined;
  creationDate: Date;
  showUsedAddresses: boolean;
};

export type WalletsLocalData = Record<StorageKey, WalletLocalData>;

export type UnpairedHardwareWalletData = {
  deviceType?: DeviceType;
  deviceModel?: string;
  deviceName?: string;
  path?: string | null | undefined;
  paired?: string | null | undefined;
  disconnected?: boolean;
};

export type HardwareWalletLocalData = {
  id: string;
  deviceType: DeviceType;
  device: TransportDevice;
  extendedPublicKey: HardwareWalletExtendedPublicKeyResponse;
  disconnected: boolean;
};

export type HardwareWalletsLocalData = Record<string, HardwareWalletLocalData>;
export type HardwareWalletDevicesType = Record<string, TransportDevice>;

export type AssetLocalData = {
  decimals?: number;
};
