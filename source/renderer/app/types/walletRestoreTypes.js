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

export type WalletSubKind =
  | WalletDaedalusKind
  | WalletYoroiKind
  | WalletHardwareKind;

export type WalletKinds = WalletKind | WalletSubKind;

export type HardwareWalletAcceptance =
  | 'hardwareWalletAcceptance1'
  | 'hardwareWalletAcceptance2';

export type WalletRestoreDataParam =
  | 'walletKind'
  | 'walletSubKind'
  | 'mnemonics'
  | 'walletName'
  | 'spendingPassword';

export type WalletRestoreData = {
  walletKind: WalletKind,
  walletKindDaedalus?: WalletDaedalusKind,
  walletKindYoroi?: WalletYoroiKind,
  walletKindHardware?: WalletHardwareKind,
  walletSubKind: WalletSubKind,
  mnemonics: Array<string>,
  walletName: string,
  spendingPassword: string,
};
