// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type restoreAdaWalletQueryParams = {
  passphrase: ?string,
};

export type restoreAdaWalletRawBodyParams = {
  walletInitData: {
    cwInitMeta: {
      cwName: string,
      cwAssurance: string,
      cwUnit: number,
    },
    cwBackupPhrase: {
      bpToList: [],
    }
  }
};

export const restoreAdaWallet = (
  ca: string,
  pathParams: {},
  queryParams: restoreAdaWalletQueryParams,
  rawBodyParams: restoreAdaWalletRawBodyParams,
): Promise<ApiWallet> => {
  const { walletInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
    port: 8090,
    ca,
  }, queryParams, walletInitData);
};
