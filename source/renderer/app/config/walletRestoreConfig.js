// @flow

import type {
  RestoreWalletStep,
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
} from '../types/walletRestoreTypes';

export const RESTORE_WALLET_STEPS: Array<RestoreWalletStep> = [
  'type',
  'mnemonics',
  'configuration',
  'success',
];

export const WALLET_KINDS: { [key: string]: WalletKind } = {
  DAEDALUS: 'Daedalus',
  YOROI: 'Yoroi',
  HARDWARE: 'Hardware',
};

export const WALLET_DAEDALUS_KINDS: { [key: string]: WalletDaedalusKind } = {
  BALANCE_12_WORD: 'Balance12Word',
  REWARD_15_WORD: 'Reward15Word',
  BALANCE_27_WORD: 'Balance27Word',
};

export const WALLET_YOROI_KINDS: { [key: string]: WalletYoroiKind } = {
  BALANCE_15_WORD: 'Balance15Word',
  REWARD_15_WORD: 'Reward15Word',
};

export const WALLET_HARDWARE_KINDS: { [key: string]: WalletHardwareKind } = {
  NANO: 'Nano',
  TREZOR: 'Trezor',
};
