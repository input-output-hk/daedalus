import type { RequestConfig } from '../../common/types';
import type { GetDelegationFeeRequest } from '../types';
import type { TransactionFee } from '../../transactions/types';
import { request } from '../../utils/request';
import { walletInputSelectionChannel } from '../../../ipc/collateral';

export const getDelegationFee = async (
  config: RequestConfig,
  { walletId }: GetDelegationFeeRequest
): Promise<TransactionFee> =>
  request(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/delegation-fees`,
      ...config,
    },
    {
      preferred_collateral: JSON.stringify(
        (await walletInputSelectionChannel.request({ walletId }))
          .preferred_collateral
      ),
    }
  );
