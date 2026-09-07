import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const submitTransaction = (
  config: RequestConfig,
  { walletId, transaction }: { walletId: string; transaction: string }
): Promise<{ id: string }> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions-submit`,
      ...config,
    },
    {},
    { transaction }
  );
