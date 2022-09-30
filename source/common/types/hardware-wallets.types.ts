import { BridgeInfo, Device as TrezorDevice, UdevInfo } from '@trezor/connect';

export type BIP32Path = Array<number>;
export type LedgerModel = 'nanoS' | 'nanoSP' | 'nanoX';
export type TrezorModel = '1' | 'T';
export type DeviceType = 'ledger' | 'trezor';
export type DeviceEvent =
  | 'device-connect'
  | 'device-connect_unacquired'
  | 'device-disconnect'
  | 'device-changed'
  | 'device-acquire'
  | 'device-release'
  | 'device-acquired'
  | 'device-released'
  | 'device-used_elsewhere'
  | 'device-loading'
  | 'button'
  | 'pin'
  | 'passphrase'
  | 'passphrase_on_device'
  | 'word'
  | 'device-wait_for_selection'
  | 'unreadable-device';
export const DeviceModels: {
  LEDGER_NANO_S: LedgerModel;
  LEDGER_NANO_S_PLUS: LedgerModel;
  LEDGER_NANO_X: LedgerModel;
  TREZOR_ONE: TrezorModel;
  TREZOR_T: TrezorModel;
} = {
  LEDGER_NANO_S: 'nanoS',
  LEDGER_NANO_S_PLUS: 'nanoSP',
  LEDGER_NANO_X: 'nanoX',
  TREZOR_ONE: '1',
  TREZOR_T: 'T',
};
export enum DeviceTypes {
  LEDGER = 'ledger',
  TREZOR = 'trezor',
}
export const DeviceEvents: {
  CONNECT: DeviceEvent;
  CONNECT_UNACQUIRED: DeviceEvent;
  DISCONNECT: DeviceEvent;
  CHANGED: DeviceEvent;
  ACQUIRE: DeviceEvent;
  RELEASE: DeviceEvent;
  ACQUIRED: DeviceEvent;
  RELEASED: DeviceEvent;
  USED_ELSEWHERE: DeviceEvent;
  LOADING: DeviceEvent;
  BUTTON: DeviceEvent;
  PIN: DeviceEvent;
  PASSPHRASE: DeviceEvent;
  PASSPHRASE_ON_DEVICE: DeviceEvent;
  WORD: DeviceEvent;
  WAIT_FOR_SELECTION: DeviceEvent;
  UNREADABLE: DeviceEvent;
} = {
  CONNECT: 'device-connect',
  CONNECT_UNACQUIRED: 'device-connect_unacquired',
  DISCONNECT: 'device-disconnect',
  CHANGED: 'device-changed',
  ACQUIRE: 'device-acquire',
  RELEASE: 'device-release',
  ACQUIRED: 'device-acquired',
  RELEASED: 'device-released',
  USED_ELSEWHERE: 'device-used_elsewhere',
  LOADING: 'device-loading',
  BUTTON: 'button',
  PIN: 'pin',
  PASSPHRASE: 'passphrase',
  PASSPHRASE_ON_DEVICE: 'passphrase_on_device',
  WORD: 'word',
  WAIT_FOR_SELECTION: 'device-wait_for_selection',
  UNREADABLE: 'unreadable-device',
};
export type AddressTypeNibble = 0 | 4 | 6 | 8 | 14;

export type LedgerTransportDevice = {
  deviceId: string | null | undefined;
  // @TODO - mark as mandatory parameter once Ledger improver
  deviceType: 'ledger';
  deviceModel: string;
  deviceName: string;
  path: string | null | undefined;
};

export type TrezorTransportDevice = {
  deviceId: string | null | undefined;
  // @TODO - mark as mandatory parameter once Ledger improver
  deviceType: 'trezor';
  deviceModel: string;
  deviceName: string;
  path: string | null | undefined;
  firmwareVersion: string | null | undefined;
};

export type TransportDevice = LedgerTransportDevice | TrezorTransportDevice;

export type Certificate = {
  address: string;
  type: string;
  accountAddress: string;
  poolHash: string | null | undefined;
  encodeCBOR: (...args: Array<any>) => any;
};
export type Withdrawal = {
  path: BIP32Path;
  amountStr: string;
};
export type LedgerSignTransactionInputType = {
  txHashHex: string;
  outputIndex: number;
  path: BIP32Path;
};
export type LedgerOutputTypeAddress = {
  amountStr: string;
  addressHex: string;
};
export type LedgerOutputTypeChange = {
  addressTypeNibble: AddressTypeNibble;
  spendingPath: BIP32Path;
  amountStr: string;
  stakingPath: BIP32Path | null | undefined;
  stakingKeyHashHex: string | null | undefined;
  stakingBlockchainPointer: StakingBlockchainPointer | null | undefined;
};
export type StakingBlockchainPointer = {
  blockIndex: number;
  txIndex: number;
  certificateIndex: number;
};
export type LedgerSignTransactionInputsType = Array<
  LedgerSignTransactionInputType
>;
export type LedgerSignTransactionOutputsType =
  | []
  | Array<LedgerOutputTypeAddress | LedgerOutputTypeChange>;
export type LedgerAuxiliaryDataType = {
  type: string;
  params: {
    votingPublicKeyHex: string;
    stakingPath: BIP32Path;
    rewardsDestination: {
      type: number;
      params: {
        stakingPath: BIP32Path;
      };
    };
    nonce: string;
  };
};
export type Witness = {
  path: BIP32Path;
  witnessSignatureHex: string;
};
export type HardwareWalletTransportDeviceRequest = {
  isTrezor: boolean;
  devicePath: string | null | undefined;
  reset?: boolean;
};
export type HardwareWalletTransportDeviceResponse = TransportDevice;
export type HardwareWalletExtendedPublicKeyRequest = {
  path: string;
  devicePath: string | null | undefined;
  isTrezor: boolean;
};
export type HardwareWalletExtendedPublicKeyResponse = {
  publicKeyHex: string;
  chainCodeHex: string;
  deviceId?: string;
};
export type HardwareWalletCardanoAdaAppResponse = {
  major: string;
  minor: string;
  patch: string;
  deviceId: string;
};
export type LedgerSignTransactionRequest = {
  inputs: LedgerSignTransactionInputsType;
  outputs: LedgerSignTransactionOutputsType;
  fee: string;
  ttl?: string;
  networkId: number;
  protocolMagic: number;
  certificates: Array<Certificate | null | undefined>;
  withdrawals: Array<Withdrawal | null | undefined>;
  reset?: boolean;
  devicePath: string | null | undefined;
  validityIntervalStartStr?: string | null | undefined;
  // It is disabled for now
  signingMode: string;
  auxiliaryData: LedgerAuxiliaryDataType | null | undefined;
  additionalWitnessPaths: Array<BIP32Path | null | undefined>;
};
export type LedgerSignTransactionResponse = {
  txHashHex: string;
  witnesses: Array<Witness>;
  auxiliaryDataSupplement?: {
    catalystRegistrationSignatureHex: string;
    auxiliaryDataHashHex: string;
    type: 'catalyst_registration';
  };
};
export type TrezorWitness = {
  type: number;
  pubKey: string;
  signature: string;
  chainCode: string | null | undefined;
};

export type LedgerDevicePayload = {
  disconnected: boolean;
  deviceType: 'ledger';
  deviceId: string | null;
  deviceModel: string;
  deviceName: string;
  path: string;
  product: string;
};

export type TrezorDevicePayload = {
  disconnected: boolean;
  deviceId: string;
  deviceType: 'trezor';
  deviceModel: TrezorDevice;
  // e.g. "1" or "T"
  deviceName: string;
  path: string;
  eventType: string;
};

export type TrezorDeviceErrorPayload = {
  deviceType: 'trezor';
  error?: {
    payload: {
      error: string;
      bridge?: BridgeInfo;
      udev?: UdevInfo;
      code?: string;
    };
  };
};

// FIXME: This should be LedgerDevicePayload | TrezorDevicePayload | TrezorDeviceErrorPayload
// Changing so will result in a huge refactoring on HardwareWalletsStore._changeHardwareWalletConnectionStatus
export type HardwareWalletConnectionRequest = {
  disconnected: boolean;
  deviceType: DeviceType;
  deviceId: string | null | undefined;
  deviceModel: string;
  deviceName: string;
  path: string | null | undefined;
  product: string | null;
  error?: {
    payload: {
      code: string;
      error: string;
    };
  };
  eventType?: DeviceEvent;
};
