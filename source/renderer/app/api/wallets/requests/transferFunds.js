// @flow
import type { RequestConfig } from '../../common/types';
import type { TransferFundsRequest, TransferFundsResponse } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const transferFunds = (
  config: RequestConfig,
  { sourceWalletId, targetWalletId, passphrase }: TransferFundsRequest
): Promise<TransferFundsResponse> =>
  request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${getRawWalletId(
        sourceWalletId
      )}/migrations/${targetWalletId}`,
      ...config,
    },
    {},
    { passphrase }
  );
