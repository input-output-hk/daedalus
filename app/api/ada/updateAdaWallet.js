// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type updateAdaWalletPathParams = {
  walletId: string,
};

export const makePayment = (
  ca: string, pathParams: updateAdaWalletPathParams
): Promise<{}> => {
  const { walletId } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: 8090,
    ca,
  });
};
