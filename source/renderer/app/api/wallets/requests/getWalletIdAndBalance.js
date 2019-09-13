// @flow
import type { RequestConfig } from '../../common/types';
import type {
  GetWalletIdAndBalanceRequest,
  GetWalletIdAndBalanceResponse,
} from '../types';
import { request } from '../../utils/request';

export const getWalletIdAndBalance = (
  config: RequestConfig,
  { recoveryPhrase, getBalance }: GetWalletIdAndBalanceRequest
): Promise<GetWalletIdAndBalanceResponse> =>
  request(
    {
      method: 'POST',
      path: '/api/internal/calculate_mnemonic',
      ...config,
    },
    {
      read_balance: getBalance,
    },
    recoveryPhrase
  );
