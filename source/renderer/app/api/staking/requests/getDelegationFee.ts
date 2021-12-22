import type { RequestConfig } from '../../common/types';
import type { GetDelegationFeeRequest } from '../types';
import type { TransactionFee } from '../../transactions/types';
import { request } from '../../utils/request';

export const getDelegationFee = (
  config: RequestConfig,
  { walletId }: GetDelegationFeeRequest
): Promise<TransactionFee> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/delegation-fees`,
    ...config,
  });
