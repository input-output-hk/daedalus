import type BigNumber from 'bignumber.js';
import { WalletUnits } from '../../../domains/Wallet';
import type { RequestConfig } from '../../common/types';
import type {
  TransactionPaymentData,
  CoinSelectionAmount,
  TransactionWithdrawalType,
  VotingMetadataType,
} from '../types';
import type { DelegationAction } from '../../../types/stakingTypes';
import { request } from '../../utils/request';
import { walletInputSelectionChannel } from '../../../ipc/collateral';

export type PaymentsType = {
  payments: Array<TransactionPaymentData>;
  withdrawal?: TransactionWithdrawalType;
  metadata?: VotingMetadataType;
};

export type DelegationType = {
  delegation_action: {
    pool: string;
    action: DelegationAction;
  };
};
export type SelectCoinsRequestType = {
  walletId: string;
  data: PaymentsType | DelegationType;
};
export type SelectCoinsWithdrawalType = {
  stake_address: string;
  derivation_path: Array<string>;
  amount: CoinSelectionAmount;
};
export type SelectCoinsResponseType = {
  inputs: Array<{
    address: string;
    amount: CoinSelectionAmount;
    id: string;
    index: number;
    derivation_path: Array<string>;
  }>;
  outputs: Array<{
    address: string;
    amount: CoinSelectionAmount;
    derivation_path?: Array<string>;
  }>;
  change: Array<{
    address: string;
    amount: CoinSelectionAmount;
    derivation_path: Array<string>;
  }>;
  withdrawals?: Array<SelectCoinsWithdrawalType>;
  certificates?: Array<{
    pool?: string;
    certificate_type: DelegationAction;
    reward_account_path: Array<string>;
    vote?: string;
  }>;
  deposits_taken?: Array<{
    quantity: number | BigNumber;
    unit: WalletUnits.LOVELACE;
  }>;
  deposits_returned?: Array<{
    quantity: number | BigNumber;
    unit: WalletUnits.LOVELACE;
  }>;
  metadata?: string;
};
export const selectCoins = async (
  config: RequestConfig,
  { walletId, data }: SelectCoinsRequestType
): Promise<SelectCoinsResponseType> => {
  return request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/coin-selections/random`,
      ...config,
    },
    {},
    { ...data, ...(await walletInputSelectionChannel.request({ walletId })) }
  );
};
