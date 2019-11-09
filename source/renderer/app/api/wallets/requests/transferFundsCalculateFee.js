// @flow
import type { RequestConfig } from '../../common/types';
import type {
  TransferFundsCalculateFeeRequest,
  TransferFundsCalculateFeeResponse,
} from '../types';
import { request } from '../../utils/request';

export const transferFundsCalculateFee = (
  config: RequestConfig,
  { sourceWalletId }: TransferFundsCalculateFeeRequest
): Promise<TransferFundsCalculateFeeResponse> =>
  request({
    method: 'GET',
    path: `/v2/byron-wallets/${sourceWalletId}/migrations`,
    ...config,
  });
