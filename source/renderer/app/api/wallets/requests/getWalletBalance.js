// @flow
import type { RequestConfig } from '../../common/types';
import type { GetWalletBalanceResponse } from '../types';
import { request } from '../../utils/request';

export const getWalletBalance = (
  config: RequestConfig,
  encryptedSecretKey: string, // EncryptedSecretKey (hex-encoded)
): Promise<GetWalletBalanceResponse> => (
  request({
    method: 'POST',
    path: '/api/internal/query-balance',
    ...config,
  }, {}, encryptedSecretKey)
);
