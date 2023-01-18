import type { RequestConfig } from '../../common/types';
import type { ICOPublicKeyParams, Transaction } from '../types';
import { request } from '../../utils/request';

export const getICOPublicKey = (
  config: RequestConfig,
  { walletId, index, data }: ICOPublicKeyParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/keys/${index}`,
      ...config,
    },
    {},
    { ...data }
  );
