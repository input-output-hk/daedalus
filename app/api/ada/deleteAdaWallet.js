// @flow
import { request } from './lib/request';

export type DeleteAdaWalletPathParams = {
  walletId: string,
};

export const deleteAdaWallet = (
  ca: string, pathParams: DeleteAdaWalletPathParams
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
