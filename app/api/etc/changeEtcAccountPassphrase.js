// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type ChangeEtcAccountPassphraseParams = {
  ca: string,
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export type ChangeEtcAccountPassphraseResponse = string;

export const changeEtcAccountPassphrase = (
  { ca, walletId, oldPassword, newPassword }: ChangeEtcAccountPassphraseParams
): Promise<ChangeEtcAccountPassphraseResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_changePassphrase',
    params: [
      walletId,
      oldPassword || '',
      newPassword || '',
    ]
  })
);
