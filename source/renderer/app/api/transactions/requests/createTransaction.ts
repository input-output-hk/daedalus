import type { RequestConfig } from '../../common/types';
import type { Transaction, TransactionParams } from '../types';
import { request } from '../../utils/request';
import { walletInputSelectionChannel } from '../../../ipc/collateral';

export const createTransaction = async (
  config: RequestConfig,
  { walletId, data }: TransactionParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions/`,
      ...config,
    },
    {},
    { ...data, ...(await walletInputSelectionChannel.request({ walletId })) }
  );
