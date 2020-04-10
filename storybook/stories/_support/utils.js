// @flow
import hash from 'hash.js';
import faker from 'faker';
import moment from 'moment';
import { random, get } from 'lodash';
import BigNumber from 'bignumber.js';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import StakePool from '../../../source/renderer/app/domains/StakePool';
import {
  WalletTransaction,
  TransactionStates,
  TransactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import WalletAddress from '../../../source/renderer/app/domains/WalletAddress';
import { LOVELACES_PER_ADA } from '../../../source/renderer/app/config/numbersConfig';
import {
  WalletRecoveryPhraseVerificationStatuses,
  WalletRecoveryPhraseVerificationTypes,
} from '../../../source/renderer/app/stores/WalletsStore';
import type {
  TransactionType,
  TransactionState,
} from '../../../source/renderer/app/api/transactions/types';

export const generateHash = () => {
  const now = new Date().valueOf().toString();
  return hash
    .sha512()
    .update(now + random(0.1, 0.9))
    .digest('hex');
};

export const generateWallet = (
  name: string,
  amount: string,
  reward?: number = 0,
  delegatedStakePool?: StakePool,
  hasPassword?: boolean
) =>
  new Wallet({
    id: generateHash(),
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(reward).dividedBy(LOVELACES_PER_ADA),
    createdAt: new Date(),
    name,
    hasPassword,
    passwordUpdateDate: new Date(),
    syncState: { status: 'ready' },
    isLegacy: false,
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus:
      WalletRecoveryPhraseVerificationStatuses.OK,
    recoveryPhraseVerificationStatusType:
      WalletRecoveryPhraseVerificationTypes.NEVER_CHECKED,
    delegatedStakePoolId: get(delegatedStakePool, 'id'),
  });

export const generateTransaction = (
  type: TransactionType = TransactionTypes.INCOME,
  date: Date = faker.date.past(),
  amount: BigNumber = new BigNumber(faker.finance.amount()),
  state: TransactionState = TransactionStates.OK,
  hasUnresolvedIncomeAddresses: boolean = false,
  noIncomeAddresses: boolean = false
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
    },
  });

export const generateRandomTransaction = (index: number) =>
  generateTransaction(
    TransactionTypes.INCOME,
    moment()
      .subtract(index, 'days')
      .toDate(),
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
  new Promise(resolve => {
    setTimeout(() => {
      resolve(returnValue);
    }, 2000);
  });

export const isIncentivizedTestnetTheme = (currentTheme: string) =>
  currentTheme === 'incentivized-testnet';
