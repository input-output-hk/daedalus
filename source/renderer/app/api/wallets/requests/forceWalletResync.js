// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export type ForceWalletResyncParams = {
  walletId: string,
};

export const forceWalletResync = (
  config: RequestConfig,
  { walletId }: ForceWalletResyncParams
): Promise<*> =>
  request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}/tip`,
      ...config,
    },
    {},
    {
      slot_number: 0,
      epoch_number: 0,
    }
  );
