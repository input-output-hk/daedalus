import BigNumber from 'bignumber.js';
import { WalletTransaction } from '../../domains/WalletTransaction';
import { WalletUnits } from '../../domains/Wallet';
import type { DelegationAction } from '../../types/stakingTypes';
import type { ApiTokens } from '../assets/types';
import type { TransactionMetadata } from '../../types/TransactionMetadata';
import type { PathRoleIdentityType } from '../../utils/hardwareWalletUtils';

export type TransactionAmount = {
  quantity: number;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
  unit: WalletUnits.LOVELACE;
};
export type TransactionDepth = {
  quantity: number;
  unit: 'block';
};
export type TransactionInsertionBlock = {
  slot_number: number;
  epoch_number: number;
};
export type Transaction = {
  id: string;
  amount: TransactionAmount;
  fee: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  deposit: {
    quantity: number;
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
    unit: WalletUnits.LOVELACE;
  };
  inserted_at?: {
    time: Date;
    block: TransactionInsertionBlock;
  };
  pending_since?: {
    time: Date;
    block: TransactionInsertionBlock & {
      height: {
        quantity: number;
        unit: string;
      };
    };
  };
  depth: TransactionDepth;
  direction: 'outgoing' | 'incoming';
  inputs: Array<TransactionInputs>;
  outputs: Array<TransactionOutputs>;
  withdrawals: Array<TransactionWithdrawals>;
  status: TransactionState;
  metadata?: TransactionMetadata;
};
export type Transactions = Array<Transaction>;
export type TransactionInputs = {
  address: string;
  amount?: TransactionAmount;
  assets?: ApiTokens;
  id: string;
  index: number;
};
export type TransactionOutputs = {
  address: string;
  amount: TransactionAmount;
  assets?: ApiTokens;
};
export type TransactionWithdrawals = {
  stake_address: string;
  amount: TransactionAmount;
};
export type TransactionWithdrawalType = 'self' | Array<string>;
export type TransactionState = 'pending' | 'in_ledger' | 'expired';
export type TransactionAddresses = {
  from: Array<string | null | undefined>;
  to: Array<string>;
  withdrawals: Array<string>;
};
export type TransactionType = 'card' | 'expend' | 'income' | 'exchange';
// Req / Res Transaction Types
export type GetTransactionsRequest = {
  walletId: string;
  order?: 'ascending' | 'descending';
  fromDate: string | null | undefined;
  toDate: string | null | undefined;
  isLegacy: boolean;
  isHardwareWallet?: boolean; // @API TODO - Params "pending" for V2
  // searchTerm: string,
  // skip: number,
  // limit: number,
  // isFirstLoad: boolean,
  // isRestoreActive: boolean,
  // isRestoreCompleted: boolean,
  // cachedTransactions: Array<WalletTransaction>,
};
export type GetTransactionRequest = {
  walletId: string;
  transactionId: string;
};
export type GetTransactionFeeRequest = {
  walletId: string;
  address: string;
  amount: number;
  assets?: ApiTokens;
  walletBalance: BigNumber;
  availableBalance: BigNumber;
  rewardsBalance: BigNumber;
  isLegacy: boolean;
  withdrawal?: 'self' | Array<string>;
};
export type GetTransactionFeeResponse = {
  fee: BigNumber;
  minimumAda: BigNumber;
};
export type CreateTransactionRequest = {
  walletId: string;
  address: string;
  amount: number;
  passphrase: string;
  isLegacy: boolean;
  assets?: ApiTokens;
  withdrawal?: 'self' | Array<string>;
};
export type DeleteTransactionRequest = {
  walletId: string;
  transactionId: string;
  isLegacy: boolean;
};
export type GetTransactionsResponse = {
  transactions: Array<WalletTransaction>;
  total: number;
};
export type TransactionParams = {
  walletId: string;
  data: {
    payments: Array<TransactionPaymentData>;
    passphrase: string;
  };
};
export type TransactionFeeAmount = {
  quantity: number;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
  unit: WalletUnits.LOVELACE;
};
export type GetTransactionFeeParams = {
  walletId: string;
  data: {
    payments: Array<TransactionPaymentData>;
  };
};
export type TransactionPaymentData = {
  address: string;
  amount: TransactionFeeAmount;
  assets?: ApiTokens;
};
export type TransactionFee = {
  estimated_min: TransactionFeeAmount;
  estimated_max: TransactionFeeAmount;
  deposit: TransactionFeeAmount;
  minimum_coins: Array<TransactionFeeAmount>;
};
export type CoinSelectionAmount = {
  quantity: number;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'WalletUnits'.
  unit: WalletUnits.LOVELACE;
};
export type CoinSelectionInput = {
  address: string;
  amount: CoinSelectionAmount;
  id: string;
  index: number;
  derivationPath: Array<string>;
};
export type Asset = {
  policyId: string;
  assetName: string;
  quantity: number;
};
export type CoinSelectionOutput = {
  address: string;
  amount: CoinSelectionAmount;
  derivationPath: Array<string>;
  assets?: Array<Asset>;
};
export type CertificateType =
  | 'register_reward_account'
  | 'quit_pool'
  | 'join_pool';
export type CoinSelectionCertificate = {
  pool: string;
  certificateType: CertificateType;
  rewardAccountPath: Array<string>;
};
export type CoinSelectionCertificates = Array<CoinSelectionCertificate>;
export type CoinSelectionWithdrawal = {
  stakeAddress: string;
  derivationPath: Array<string>;
  amount: CoinSelectionAmount;
};
export type CoinSelectionWithdrawals = Array<CoinSelectionWithdrawal>;
export type CoinSelectionsDelegationRequestType = {
  walletId: string;
  poolId: string;
  delegationAction: DelegationAction;
};
export type CoinSelectionsPaymentRequestType = {
  walletId: string;
  address: string;
  amount: number;
  assets?: ApiTokens;
  metadata?: VotingMetadataType;
};
export type CoinSelectionsRequest =
  | CoinSelectionsPaymentRequestType
  | CoinSelectionsDelegationRequestType;
export type CoinSelectionsResponse = {
  inputs: Array<CoinSelectionInput>;
  outputs: Array<CoinSelectionOutput>;
  certificates: CoinSelectionCertificates;
  deposits: BigNumber;
  depositsReclaimed: BigNumber;
  withdrawals: Array<CoinSelectionWithdrawal>;
  fee: BigNumber;
  metadata: string | null | undefined;
};
export type CreateExternalTransactionRequest = {
  signedTransactionBlob: Buffer;
};
export type CreateExternalTransactionResponse = {
  id: string;
};
export type GetWithdrawalsRequest = {
  walletId: string;
};
export type GetWithdrawalsResponse = {
  withdrawals: BigNumber;
};
export type ICOPublicKeyParams = {
  walletId: string;
  index: string;
  data: {
    passphrase: string;
    format: 'extended' | 'non_extended';
    purpose: string;
  };
};
export type CoinSelectionAssetsType = Array<Asset>;
export type VotingMetaIndexType = 61284 | 61285;
export const VotingMetaIndexes: {
  VOTING_REGISTRATION: VotingMetaIndexType;
  VOTING_SIGNATURE: VotingMetaIndexType;
} = {
  VOTING_REGISTRATION: 61284,
  VOTING_SIGNATURE: 61285,
};
export type VotingMetaKeyValuePairString = { [key in 'string']?: string };
export type VotingMetaKeyValuePairInt = { [key in 'int']?: number };
export type VotingMetaKeyValuePairBytes = { [key in 'bytes']?: string };
export type VotingMetaKeyValuePairMap = { [key in 'int']?: number };
export type VotingMetaKeyType = 'string' | 'int' | 'bytes' | 'list' | 'map';
export type VotingMetaKeyValuePair = {
  [key in 'k' | 'v']?:
    | VotingMetaKeyValuePairString
    | VotingMetaKeyValuePairInt
    | VotingMetaKeyValuePairBytes;
};
export type VotingMetaRegistrationType = {
  [key in 'map']?: Array<VotingMetaKeyValuePair>;
};
export type VotingMetadataType = Record<
  VotingMetaIndexType,
  VotingMetaRegistrationType
>;
export type VotingDataType = {
  stakeAddress: string;
  stakeAddressHex: string;
  votingKey: string;
  stakeKey: string;
  role: PathRoleIdentityType;
  index: string;
  metadata: VotingMetadataType;
  nonce: number;
};
