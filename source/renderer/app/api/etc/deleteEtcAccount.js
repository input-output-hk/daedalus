// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type DeleteEtcAccountBalanceParams = {
  ca: string,
  walletId: string,
};

export const deleteEtcAccount = (
  { ca, walletId }: DeleteEtcAccountBalanceParams
): Promise<boolean> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_deleteWallet',
    params: [walletId]
  })
);
