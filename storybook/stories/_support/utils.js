// @flow
import hash from 'hash.js';
import faker from 'faker';
import moment from 'moment';
import { random, get } from 'lodash';
import BigNumber from 'bignumber.js';
import Wallet, {
  WalletSyncStateStatuses,
} from '../../../source/renderer/app/domains/Wallet';
import StakePool from '../../../source/renderer/app/domains/StakePool';
import {
  WalletTransaction,
  TransactionStates,
  TransactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import WalletAddress from '../../../source/renderer/app/domains/WalletAddress';
import { LOVELACES_PER_ADA } from '../../../source/renderer/app/config/numbersConfig';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../../../source/renderer/app/config/walletRecoveryPhraseVerificationConfig';
import type {
  TransactionType,
  TransactionState,
} from '../../../source/renderer/app/api/transactions/types';
import type { SyncStateStatus } from '../../../source/renderer/app/api/wallets/types';

export const generateHash = () => {
  const now = new Date().valueOf().toString();
  return hash
    .sha512()
    .update(now + random(0.1, 0.9))
    .digest('hex');
};

const statusProgress = (status) =>
  status === WalletSyncStateStatuses.RESTORING
    ? {
        progress: {
          quantity: 50,
          unit: 'percentage',
        },
      }
    : null;

export const generateWallet = (
  name: string,
  amount: string,
  reward?: number = 0,
  delegatedStakePool?: StakePool,
  hasPassword?: boolean,
  status?: SyncStateStatus = WalletSyncStateStatuses.READY
) =>
  new Wallet({
    id: generateHash(),
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(reward).dividedBy(LOVELACES_PER_ADA),
    createdAt: new Date(),
    name,
    hasPassword: hasPassword || false,
    passwordUpdateDate: new Date(),
    syncState: { status, ...statusProgress(status) },
    isLegacy: false,
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    delegatedStakePoolId: get(delegatedStakePool, 'id'),
  });

export const generateNativeTokenWallet = (
  name: string,
  amount: string,
  reward?: number = 0,
  delegatedStakePool?: StakePool,
  hasPassword?: boolean,
  status?: SyncStateStatus = WalletSyncStateStatuses.READY
) =>
  new Wallet({
    id: generateHash(),
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(reward).dividedBy(LOVELACES_PER_ADA),
    createdAt: new Date(),
    name,
    hasPassword: hasPassword || false,
    passwordUpdateDate: new Date(),
    syncState: { status, ...statusProgress(status) },
    isLegacy: false,
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
    RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    delegatedStakePoolId: get(delegatedStakePool, 'id'),
  });

export const generateTransaction = (
  type: TransactionType = TransactionTypes.INCOME,
  date: Date = faker.date.past(),
  amount: BigNumber = new BigNumber(faker.finance.amount()),
  state: TransactionState = TransactionStates.OK,
  hasUnresolvedIncomeAddresses: boolean = false,
  noIncomeAddresses: boolean = false,
  noWithdrawals: boolean = true
) =>
  new WalletTransaction({
    id: faker.random.uuid(),
    title: '',
    type,
    amount,
    date,
    state,
    depth: {
      quantity: 0,
      unit: 'block',
    },
    epochNumber: 0,
    slotNumber: 0,
    description: '',
    addresses: {
      from: noIncomeAddresses
        ? []
        : [
            hasUnresolvedIncomeAddresses
              ? ''
              : faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
          ],
      to: [
        faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
        faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
      ],
      withdrawals: noWithdrawals
        ? []
        : [
            faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
            faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
          ],
    },
  });

export const generateRandomTransaction = (index: number) =>
  generateTransaction(
    TransactionTypes.INCOME,
    moment().subtract(index, 'days').toDate(),
    new BigNumber(faker.random.number(5))
  );

export const generateMultipleTransactions = (
  amount: number
): WalletTransaction[] =>
  Array.from(Array(amount).keys()).map((key: number) =>
    generateRandomTransaction(Math.round(Math.random() * key))
  );

export const generateAddress = (used: boolean = false): WalletAddress =>
  new WalletAddress({
    id: generateHash(),
    used,
  });

export const promise = (returnValue: any): (() => Promise<any>) => () =>
  new Promise((resolve) => {
    setTimeout(() => {
      resolve(returnValue);
    }, 2000);
  });

export const isIncentivizedTestnetTheme = (currentTheme: string) =>
  currentTheme === 'incentivized-testnet';

export const isShelleyTestnetTheme = (currentTheme: string) =>
  currentTheme === 'shelley-testnet';
