// @flow

export type BIP32Path = Array<number>;
export type LedgerModel = 'nanoS' | 'nanoX';
export type TrezorModel = '1' | 'T';
export type DeviceType = 'ledger' | 'trezor';

export const DeviceModels: {
  LEDGER_NANO_S: LedgerModel,
  LEDGER_NANO_X: LedgerModel,
  TREZOR_ONE: TrezorModel,
  TREZOR_T: TrezorModel,
} = {
  LEDGER_NANO_S: 'nanoS',
  LEDGER_NANO_X: 'nanoX',
  TREZOR_ONE: '1',
  TREZOR_T: 'T',
};

export const DeviceTypes: {
  LEDGER: DeviceType,
  TREZOR: DeviceType,
} = {
  LEDGER: 'ledger',
  TREZOR: 'trezor',
};

export type AddressTypeNibble = 0b0000 | 0b0100 | 0b0110 | 0b1000 | 0b1110;

export const AddressTypeNibbles: {
  BASE: AddressTypeNibble,
  POINTER: AddressTypeNibble,
  ENTERPRISE: AddressTypeNibble,
  BYRON: AddressTypeNibble,
  REWARD: AddressTypeNibble,
} = {
  BASE: 0b0000,
  POINTER: 0b0100,
  ENTERPRISE: 0b0110,
  BYRON: 0b1000,
  REWARD: 0b1110,
};

export type CertificateType = 0 | 1 | 2;

export const CertificateTypes: {
  STAKE_REGISTRATION: CertificateType,
  STAKE_DEREGISTRATION: CertificateType,
  STAKE_DELEGATION: CertificateType,
} = {
  STAKE_REGISTRATION: 0,
  STAKE_DEREGISTRATION: 1,
  STAKE_DELEGATION: 2,
};

export type TransportDevice = {
  deviceId: ?string, // @TODO - mark as mandatory parameter once Ledger improver
  deviceType: DeviceType,
  deviceModel: string,
  deviceName: string,
  path: ?string,
  firmwareVersion: ?string,
};

/* export type Certificate = {|
  type: CertificateType,
  path: BIP32Path,
  poolKeyHashHex: ?string,
|}; */

export type Certificate = {
  address: string,
  type: string,
  accountAddress: string,
  poolHash: ?string,
  encodeCBOR: Function,
};

export type Withdrawal = {|
  path: BIP32Path,
  amountStr: string,
|};

export type LedgerSignTransactionInputType = {|
  txHashHex: string,
  outputIndex: number,
  path: BIP32Path,
|};

export type LedgerOutputTypeAddress = {|
  amountStr: string,
  addressHex: string,
|};

export type LedgerOutputTypeChange = {|
  addressTypeNibble: AddressTypeNibble,
  spendingPath: BIP32Path,
  amountStr: string,
  stakingPath: ?BIP32Path,
  stakingKeyHashHex: ?string,
  stakingBlockchainPointer: ?StakingBlockchainPointer,
|};

export type StakingBlockchainPointer = {|
  blockIndex: number,
  txIndex: number,
  certificateIndex: number,
|};

export type LedgerSignTransactionInputsType = Array<LedgerSignTransactionInputType>;

export type LedgerSignTransactionOutputsType =
  | []
  | Array<LedgerOutputTypeAddress | LedgerOutputTypeChange>;

export type TrezorSignTransactionInputType = {
  path: string,
  prev_hash: number,
  prev_index: number,
};

export type TrezorOutputTypeAddress = {
  address: string,
  amount: string,
};

export type TrezorOutputTypeChange = {
  amount: string,
  addressParameters: {
    addressType: number,
    path: string,
    stakingPath: string,
  },
};

export type TrezorSignTransactionInputsType = Array<TrezorSignTransactionInputType>;

export type TrezorSignTransactionOutputsType = Array<
  TrezorOutputTypeAddress | TrezorOutputTypeChange
>;

export type Witness = {|
  path: BIP32Path,
  witnessSignatureHex: string,
|};

export type HardwareWalletTransportDeviceRequest = {
  isTrezor: boolean,
  devicePath: ?string,
  reset?: boolean,
};

export type HardwareWalletTransportDeviceResponse = TransportDevice;

export type HardwareWalletExtendedPublicKeyRequest = {
  path: string,
  devicePath: ?string,
  isTrezor: boolean,
};

export type HardwareWalletExtendedPublicKeyResponse = {
  publicKeyHex: string,
  chainCodeHex: string,
  deviceId?: string,
};

export type HardwareWalletCardanoAdaAppResponse = {
  major: string,
  minor: string,
  patch: string,
  deviceId: string,
};

export type LedgerSignTransactionRequest = {
  inputs: LedgerSignTransactionInputsType,
  outputs: LedgerSignTransactionOutputsType,
  fee: string,
  ttl: string,
  networkId: number,
  protocolMagic: number,
  // $FlowFixMe
  certificates: Array<?Certificate>, // TODO - add once certificates defined
  // $FlowFixMe
  withdrawals: Array<?Withdrawal>, // TODO - add once withdrawals defined
  metadataHashHex: ?string, // TODO - add once metadata defined
  reset?: boolean,
  devicePath: ?string,
};

export type TrezorSignTransactionRequest = {
  inputs: TrezorSignTransactionInputsType,
  outputs: TrezorSignTransactionOutputsType,
  fee: string,
  ttl: string,
  networkId: number,
  protocolMagic: number,
  certificates: Array<?Certificate>,
  reset?: boolean,
  devicePath: string,
};

export type LedgerSignTransactionResponse = {
  txHashHex: string,
  witnesses: Array<Witness>,
};

export type TrezorSignTransactionResponse = {
  success: boolean,
  payload: {
    serializedTx: string,
  },
};

export type HardwareWalletConnectionRequest = {
  disconnected: boolean,
  deviceType: DeviceType,
  deviceId: ?string,
  deviceModel: string,
  deviceName: string,
  path: ?string,
  error?: {
    payload: {
      code: string,
      error: string,
    },
  },
};
