// @flow
import { WalletUnits } from '../../../domains/Wallet';
import type { RequestConfig } from '../../common/types';
import type { TransactionPaymentData, CoinSelectionAmount } from '../types';
import type { DelegationAction } from '../../../types/stakingTypes';
import { request } from '../../utils/request';

export type PaymentsType = {
  payments: Array<TransactionPaymentData>,
};

export type DelegationType = {
  delegation_action: {
    pool: string,
    action: DelegationAction,
  },
};

export type SelectCoinsRequestType = {
  walletId: string,
  data: PaymentsType | DelegationType,
};

export type SelectCoinsResponseType = {
  inputs: Array<{
    address: string,
    amount: CoinSelectionAmount,
    id: string,
    index: number,
    derivation_path: Array<string>,
  }>,
  outputs: Array<{
    address: string,
    amount: CoinSelectionAmount,
    derivation_path?: Array<string>,
  }>,
  change: Array<{
    address: string,
    amount: CoinSelectionAmount,
    derivation_path: Array<string>,
  }>,
  certificates?: Array<{
    pool?: string,
    certificate_type: DelegationAction,
    reward_account_path: Array<string>,
  }>,
  deposits?: Array<{
    quantity: number,
    unit: WalletUnits.LOVELACE,
  }>,
};

export const selectCoins = (
  config: RequestConfig,
  { walletId, data }: SelectCoinsRequestType
): Promise<SelectCoinsResponseType> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/coin-selections/random`,
      ...config,
    },
    {},
    data
  );
