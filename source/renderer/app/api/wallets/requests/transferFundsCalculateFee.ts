import type { RequestConfig } from '../../common/types';
import type {
  TransferFundsCalculateFeeRequest,
  TransferFundsCalculateFeeApiResponse,
} from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const transferFundsCalculateFee = (
  config: RequestConfig,
  { sourceWalletId }: TransferFundsCalculateFeeRequest
): Promise<TransferFundsCalculateFeeApiResponse> =>
  request({
    method: 'GET',
    path: `/v2/byron-wallets/${getRawWalletId(sourceWalletId)}/migrations`,
    ...config,
  });
