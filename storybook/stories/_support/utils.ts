import hash from 'hash.js';
import faker from '@faker-js/faker';
import JSONBigInt from 'json-bigint';
import moment from 'moment';
import { get } from 'lodash';
import BigNumber from 'bignumber.js';
import seedrandom from 'seedrandom';
import type { Reward } from '../../../source/renderer/app/api/staking/types';
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
import Asset from '../../../source/renderer/app/domains/Asset';
import type {
  AssetMetadata,
  Tokens,
  WalletTokens,
  AssetToken,
} from '../../../source/renderer/app/api/assets/types';
import { hexToString } from '../../../source/renderer/app/utils/strings';
import type { SyncStateStatus } from '../../../source/renderer/app/api/wallets/types';
import type { TransactionMetadata } from '../../../source/renderer/app/types/TransactionMetadata';

const random = seedrandom('daedalus', {
  global: false,
});
export const EXAMPLE_METADATA = JSONBigInt.parse(`{
      "0": {
        "string": "some string"
      },
      "1": {
        "int": 99999999999999999999999
      },
      "2": {
        "bytes": "2512a00e9653fe49a44a5886202e24d77eeb998f"
      },
      "3": {
        "list": [
          { "int": 14 },
          { "int": 42 },
          { "string": "1337" },
          { "list": [
            { "string": "nested list" },
            { "string": "" }
          ]}
        ]
      },
      "4": {
        "map": [
          {
            "k": { "int": "5" },
            "v": { "bytes": "2512a00e9653fe49a44a5886202e24d77eeb998f" }
          },
          {
            "k": { "map": [
              {
                "k": { "int": 14 },
                "v": { "int": 42 }
              }
            ]},
            "v": { "string": "nested" }
          },
          {
            "k": { "string": "key" },
            "v": { "list": [
            { "string": "nested list" }
          ] }
          }
        ]
      }
    }`);
export const generateHash = () => {
  return hash.sha512().update(random().toString()).digest('hex');
};
export const generatePolicyIdHash = () => {
  return hash.sha224().update(random().toString()).digest('hex');
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
  assets: WalletTokens = {
    available: [],
    total: [],
  },
  reward: string | number = 0,
  delegatedStakePool: StakePool = null,
  hasPassword = false,
  status: SyncStateStatus = WalletSyncStateStatuses.READY,
  isHardwareWallet = false,
  id: string = generateHash()
) =>
  new Wallet({
    id,
    addressPoolGap: 20,
    amount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    availableAmount: new BigNumber(amount).dividedBy(LOVELACES_PER_ADA),
    reward: new BigNumber(reward).dividedBy(LOVELACES_PER_ADA),
    assets,
    createdAt: new Date(),
    name,
    hasPassword: hasPassword || false,
    passwordUpdateDate: new Date(),
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ progress: { quantity: number; unit: string... Remove this comment to see the full error message
    syncState: {
      status,
      ...statusProgress(status),
    },
    isLegacy: false,
    isHardwareWallet,
    discovery: 'random',
    recoveryPhraseVerificationDate: new Date(),
    recoveryPhraseVerificationStatus: RECOVERY_PHRASE_VERIFICATION_STATUSES.OK,
    recoveryPhraseVerificationStatusType:
      RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED,
    delegatedStakePoolId: get(delegatedStakePool, 'id'),
  });
export const generateRewardForWallet = (
  wallet: Wallet,
  unspent: string | number = '0'
): Reward => ({
  wallet: wallet.name,
  total: wallet.reward,
  unspent: new BigNumber(unspent).dividedBy(LOVELACES_PER_ADA),
  isRestoring: wallet.isRestoring,
  syncingProgress: 100,
  rewardsAddress: 'stake_fake_address',
});
export const generateAssetDomain = (
  policyId: string,
  assetName = '',
  fingerprint = '',
  // @ts-ignore ts-migrate(2739) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  metadata: AssetMetadata = {}
): Asset =>
  new Asset({
    policyId,
    assetName,
    fingerprint,
    metadata,
    decimals: 0,
    recommendedDecimals: null,
    uniqueId: `${policyId}${assetName}`,
  });
export const generateAssetToken = (
  policyId: string,
  assetName = '',
  fingerprint = '',
  quantity = 0,
  metadata?: AssetMetadata | null,
  decimals?: number | null,
  recommendedDecimals?: number | null
): AssetToken => ({
  policyId,
  assetName,
  assetNameASCII: hexToString(assetName),
  fingerprint,
  metadata,
  quantity: new BigNumber(quantity),
  decimals,
  recommendedDecimals,
  uniqueId: `${policyId}${assetName}`,
});
export const generateTransaction = (
  type: TransactionType = TransactionTypes.INCOME,
  date: Date = faker.date.past(),
  amount: BigNumber = new BigNumber(faker.finance.amount()),
  deposit: BigNumber = new BigNumber(faker.finance.amount()),
  state: TransactionState = TransactionStates.OK,
  hasUnresolvedIncomeAddresses = false,
  noIncomeAddresses = false,
  noWithdrawals = true,
  fee: BigNumber = new BigNumber(faker.finance.amount()),
  assets?: Tokens,
  metadata: TransactionMetadata = EXAMPLE_METADATA
) =>
  new WalletTransaction({
    id: faker.datatype.uuid(),
    title: '',
    type,
    amount: amount.plus(fee),
    fee,
    assets: assets || [],
    deposit,
    date,
    state,
    confirmations: 0,
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
    metadata,
  });
export const generateRandomTransaction = (index: number) =>
  generateTransaction(
    TransactionTypes.INCOME,
    moment().subtract(index, 'days').toDate(),
    new BigNumber(faker.datatype.number(5))
  );
export const generateMultipleTransactions = (
  amount: number
): WalletTransaction[] =>
  Array.from(Array(amount).keys()).map((key: number) =>
    generateRandomTransaction(Math.round(Math.random() * key))
  );
export const generateAddress = (used = false): WalletAddress =>
  new WalletAddress({
    id: generateHash(),
    used,
    spendingPath: "1852'/1815'/0'/0/19",
  });
export const promise = (returnValue: any): (() => Promise<any>) => () =>
  new Promise((resolve) => {
    setTimeout(() => {
      resolve(returnValue);
    }, 2000);
  });
export const isShelleyTestnetTheme = (currentTheme: string) =>
  currentTheme === 'shelley-testnet';
