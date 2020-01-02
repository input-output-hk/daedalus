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
  LEDGER: 'Ledger',
  TREZOR: 'Trezor',
};

export const WALLET_DAEDALUS_WORD_COUNT: {
  [key: WalletDaedalusKind]: number,
} = {
  [WALLET_DAEDALUS_KINDS.BALANCE_12_WORD]: 12,
  [WALLET_DAEDALUS_KINDS.REWARD_15_WORD]: 15,
  [WALLET_DAEDALUS_KINDS.BALANCE_27_WORD]: 27,
};

export const WALLET_YOROI_WORD_COUNT: { [key: WalletYoroiKind]: number } = {
  [WALLET_YOROI_KINDS.BALANCE_15_WORD]: 15,
  [WALLET_YOROI_KINDS.REWARD_15_WORD]: 15,
};

export const WALLET_HARDWARE_WORD_COUNT: {
  [key: WalletHardwareKind]: Array<number>,
} = {
  [WALLET_HARDWARE_KINDS.LEDGER]: [12, 18, 24],
  [WALLET_HARDWARE_KINDS.TREZOR]: [12, 18, 24],
};
