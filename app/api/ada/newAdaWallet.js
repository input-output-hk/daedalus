// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type NewAdaWalletQueryParams = {
  passphrase: ?string,
};

export type NewAdaWalletRawBodyParams = {
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

export const newAdaWallet = (
  ca: string,
  pathParams: {},
  queryParams: NewAdaWalletQueryParams,
  rawBodyParams: NewAdaWalletRawBodyParams,
): Promise<ApiWallet> => {
  const { walletInitData } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: 8090,
    ca,
  }, queryParams, walletInitData);
};
