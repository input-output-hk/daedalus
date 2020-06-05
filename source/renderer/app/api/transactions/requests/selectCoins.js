// @flow
import type { RequestConfig } from '../../common/types';
import type { CoinSelectionsResponse, TransactionPaymentData } from '../types';
import { request } from '../../utils/request';

export type SelectCoinsParams = {
  walletId: string,
  data: {
    payments: Array<TransactionPaymentData>,
  },
};

export const selectCoins = (
  config: RequestConfig,
  { walletId, data }: SelectCoinsParams
): Promise<CoinSelectionsResponse> =>
  request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${walletId}/coin-selections/random`,
      ...config,
    },
    {},
    data
  );
