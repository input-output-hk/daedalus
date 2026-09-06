import type { RequestConfig } from '../../common/types';
import type { TransactionFee, GetTransactionFeeParams } from '../types';
import { request } from '../../utils/request';
import { walletInputSelectionChannel } from '../../../ipc/collateral';

export const getTransactionFee = async (
  config: RequestConfig,
  { walletId, data }: GetTransactionFeeParams
): Promise<TransactionFee> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/payment-fees`,
      ...config,
    },
    {},
    { ...data, ...(await walletInputSelectionChannel.request({ walletId })) }
  );
