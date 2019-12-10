// @flow
import type { RequestConfig } from '../../common/types';
import type { DelegationFee, GetDelegationFeeRequest } from '../types';
import { request } from '../../utils/request';

export const getDelegationFee = (
  config: RequestConfig,
  { walletId }: GetDelegationFeeRequest
): Promise<DelegationFee> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/delegations/fees`,
    ...config,
  });
