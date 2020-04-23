// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const forceLegacyWalletResync = (
  config: RequestConfig,
  { walletId }: { walletId: string }
): Promise<*> =>
  request(
    {
      method: 'PUT',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}/tip`,
      ...config,
    },
    {},
    {
      slot_number: 0,
      epoch_number: 0,
    }
  );
