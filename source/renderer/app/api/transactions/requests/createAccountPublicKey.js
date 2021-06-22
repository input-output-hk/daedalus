// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import { request } from '../../utils/request';

export type CreateAccountPublicKeyParams = {
  walletId: string,
  index: string,
  data: {
    passphrase: string,
    format: 'extended' | 'non_extended',
    purpose: string,
  },
};

export const createAccountPublicKey = (
  config: RequestConfig,
  { walletId, index, data }: CreateAccountPublicKeyParams
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
