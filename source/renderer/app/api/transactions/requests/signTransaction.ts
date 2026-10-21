import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const signTransaction = (
  config: RequestConfig,
  {
    walletId,
    transaction,
    passphrase,
  }: { walletId: string; transaction: string; passphrase: string }
): Promise<{ transaction: string }> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions-sign`,
      ...config,
    },
    {},
    { transaction, passphrase, encoding: 'base16' }
  );
