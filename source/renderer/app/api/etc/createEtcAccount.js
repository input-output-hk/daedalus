// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcWalletId } from './types';

export type CreateEtcAccountParams = {
  ca: string,
  privateKey: string,
  password: ?string,
};

export const createEtcAccount = (
  { ca, privateKey, password }: CreateEtcAccountParams
): Promise<EtcWalletId> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'personal_importRawKey',
    params: [
      privateKey,
      password || '',
    ]
  })
);
