// @flow
import type { RequestConfig } from '../../common/types';
import type { TransferFundsRequest, TransferFundsResponse } from '../types';
import { request } from '../../utils/request';

export const transferFunds = (
  config: RequestConfig,
  { sourceWalletId, targetWalletId, passphrase }: TransferFundsRequest
): Promise<TransferFundsResponse> =>
  request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${sourceWalletId}/migrations/${targetWalletId}`,
      ...config,
    },
    {},
    { passphrase }
  );
