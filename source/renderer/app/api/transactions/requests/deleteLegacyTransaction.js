// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const deleteLegacyTransaction = (
  config: RequestConfig,
  { walletId, transactionId }: { walletId: string, transactionId: string }
): Promise<*> =>
  request({
    method: 'DELETE',
    path: `/v2/byron-wallets/${getRawWalletId(
      walletId
    )}/transactions/${transactionId}`,
    ...config,
  });
