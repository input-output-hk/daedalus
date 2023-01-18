export type RestoreWalletStep =
  | 'type'
  | 'mnemonics'
  | 'configuration'
  | 'success';
export type ImportWalletStep = 'WalletImportFile' | 'WalletSelectImport';
export type WalletKind = 'Daedalus' | 'Yoroi' | 'Hardware';
export type WalletDaedalusKind =
  | '12WordByron'
  | '15WordShelley'
  | '24WordShelley'
  | '27WordPaper';
export type WalletYoroiKind = '15WordByron' | '15WordShelley';
export type WalletByronKind = 'icarus' | 'ledger' | 'random' | 'trezor';
export type WalletHardwareKind = 'Ledger' | 'Trezor';
export type WalletSubKind =
  | WalletDaedalusKind
  | WalletYoroiKind
  | WalletHardwareKind;
export type WalletKinds = WalletKind | WalletSubKind;
export type HardwareWalletAcceptance =
  | 'hardwareWalletAcceptance1'
  | 'hardwareWalletAcceptance2'
  | 'hardwareWalletAcceptance3';
export type WalletRestoreDataParam =
  | 'walletKind'
  | 'walletSubKind'
  | 'mnemonics'
  | 'walletName'
  | 'spendingPassword';
export type WalletRestoreData = {
  walletKind: WalletKind;
  walletKindDaedalus?: WalletDaedalusKind;
  walletKindYoroi?: WalletYoroiKind;
  walletKindHardware?: WalletHardwareKind;
  walletSubKind: WalletSubKind;
  mnemonics: Array<string>;
  walletName: string;
  spendingPassword: string;
};
