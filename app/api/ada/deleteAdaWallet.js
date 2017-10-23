// @flow
import { request } from './lib/request';

export type deleteAdaWalletPathParams = {
  walletId: string,
};

export const deleteAdaWallet = (
  ca: string, pathParams: deleteAdaWalletPathParams
): Promise<[]> => {
  const { walletId } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
    port: 8090,
    ca,
  });
};
