// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import { request } from '../../utils/request';

export type ICOPublicKeyParams = {
  walletId: string,
  index: string,
  data: {
    passphrase: string,
    format: 'extended' | 'non_extended',
    purpose: string,
  },
};

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
    data
  );
