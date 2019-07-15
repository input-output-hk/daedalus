// @flow
import type { RequestConfig } from '../../common/types';
import type { Address } from '../types';
import { request } from '../../utils/request';

export type CreateAddressParams = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

// NOTE: There is no longer a construct of "create address"
// We can still present it to the user as such, but we just take
// the first unused address from this endpoint: /wallets/${walletId}/addresses?state=unused
export const createAddress = (
  config: RequestConfig,
  { spendingPassword, accountIndex, walletId }: CreateAddressParams
): Promise<Address> =>
  request(
    {
      method: 'POST',
      path: '/api/v1/addresses',
      ...config,
    },
    {},
    { spendingPassword, accountIndex, walletId }
  );
