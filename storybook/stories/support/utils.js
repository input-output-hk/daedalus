// @flow
import hash from 'hash.js';
import faker from 'faker';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import {
  WalletTransaction,
  transactionStates,
  transactionTypes,
} from '../../../source/renderer/app/domains/WalletTransaction';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import WalletAddress from '../../../source/renderer/app/domains/WalletAddress';
import { LOVELACES_PER_ADA } from '../../../source/renderer/app/config/numbersConfig';
import type {
  TransactionState,
  TransactionType,
} from '../../../source/renderer/app/api/transactions/types';

export const generateHash = () => {
  const now = new Date().valueOf().toString();
  const random = Math.random().toString();
  return hash
    .sha512()
    .update(now + random)
    .digest('hex');
};

export const generateWallet = (name: string, amount: string) =>
  new Wallet({
    id: generateHash(),
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    name,
    hasPassword: false,
    passwordUpdateDate: new Date(),
    syncState: { status: 'ready' },
    isLegacy: false,
    isDelegated: false,
  });

export const generateTransaction = (
  type: TransactionType = transactionTypes.INCOME,
  date: Date = faker.date.past(),
  amount: BigNumber = new BigNumber(faker.finance.amount()),
  confirmations: number = 1,
  state: TransactionState = transactionStates.OK
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
      unit: 'slot',
    },
    epochNumber: 0,
    slotNumber: 0,
    description: '',
    numberOfConfirmations: confirmations,
    addresses: {
      from: [faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100)],
      to: [
        faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
        faker.random.alphaNumeric(Math.round(Math.random() * 10) + 100),
      ],
    },
  });

export const generateRandomTransaction = (index: number) =>
  generateTransaction(
    transactionTypes.INCOME,
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
