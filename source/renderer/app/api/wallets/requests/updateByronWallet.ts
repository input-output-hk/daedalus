import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const updateByronWallet = (
  config: RequestConfig,
  {
    walletId,
    name,
  }: {
    walletId: string;
    name: string;
  }
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}`,
      ...config,
    },
    {},
    {
      name,
    }
  );
