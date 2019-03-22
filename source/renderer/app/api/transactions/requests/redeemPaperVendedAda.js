// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import { request } from '../../utils/request';

export type RedeemPaperVendedAdaParams = {
  redemptionCode: string,
  mnemonic: Array<string>,
  spendingPassword?: string,
  walletId: string,
  accountIndex: number,
};

export const redeemPaperVendedAda = (
  config: RequestConfig,
  redemptionParams: RedeemPaperVendedAdaParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: '/api/v1/transactions/certificates',
      ...config,
    },
    {},
    redemptionParams
  );
