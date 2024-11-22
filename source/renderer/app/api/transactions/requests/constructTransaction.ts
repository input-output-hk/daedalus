import type { RequestConfig } from '../../common/types';
import {
  ConstructTransactionData,
  ConstructTransactionResponse,
} from '../types';
import { request } from '../../utils/request';

export const constructTransaction = (
  config: RequestConfig,
  { walletId, data }: ConstructTransactionData
): Promise<ConstructTransactionResponse> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions-construct`,
      ...config,
    },
    {},
    data
  );
