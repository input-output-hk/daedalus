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

export type TransportDevice = {
  deviceID: ?string,
  deviceType: DeviceType,
  deviceModel: string,
  deviceName: string,
};

export type LedgerSignTransactionInputType = {
  txDataHex: string,
  outputIndex: number,
  path: BIP32Path,
};

export type LedgerOutputTypeAddress = {
  amountStr: string,
  address58: string,
};

export type LedgerOutputTypeChange = {
  amountStr: string,
  path: BIP32Path,
};

export type LedgerSignTransactionInputsType = Array<LedgerSignTransactionInputType>;

export type LedgerSignTransactionOutputsType = Array<
  LedgerOutputTypeAddress | LedgerOutputTypeChange
>;

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
  isTrezor: string,
};

export type HardwareWalletTransportDeviceResponse = TransportDevice;

export type HardwareWalletExtendedPublicKeyRequest = {
  path: string,
  isTrezor: string,
};

export type HardwareWalletExtendedPublicKeyResponse = {
  publicKeyHex: string,
  chainCodeHex: string,
};

export type HardwareWalletCardanoAdaAppResponse = {
  major: string,
  minor: string,
  patch: string,
};

export type HardwareWalletSignTransactionRequest = {
  inputs: TrezorSignTransactionInputsType | LedgerSignTransactionInputsType,
  outputs: TrezorSignTransactionOutputsType | LedgerSignTransactionOutputsType,
  fee: string,
  ttl: string,
  networkId: number,
  protocolMagic: number,
  isTrezor: boolean,
  certificates?: Array<?any>, // TODO - add once certificates defined
  withdrawals?: Array<?any>, // TODO - add once withdrawals defined
  metadataHashHex?: ?string, // TODO - add once metadata defined
};

export type SignTransactionLedgerResponse = {
  txHashHex: string,
  witnesses: Array<Witness>,
};

export type SignTransactionTrezorResponse = {
  serializedTx: string,
};

export type HardwareWalletSignTransactionResponse = SignTransactionLedgerResponse | SignTransactionTrezorResponse;
