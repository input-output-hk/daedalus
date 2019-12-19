// @flow

export type RestoreWalletStep =
  | 'type'
  | 'mnemonics'
  | 'configuration'
  | 'success';

export type WalletKind = 'Daedalus' | 'Yoroi' | 'Hardware';
export type WalletDaedalusKind =
  | 'Balance12Word'
  | 'Reward15Word'
  | 'Balance27Word';
export type WalletYoroiKind = 'Balance15Word' | 'Reward15Word';
export type WalletHardwareKind = 'Nano' | 'Trezor';
export type WalletKinds =
  | WalletKind
  | WalletDaedalusKind
  | WalletYoroiKind
  | WalletHardwareKind;

export type hardwareWalletAcceptance =
  | 'hardwareWalletAcceptance1'
  | 'hardwareWalletAcceptance2';
