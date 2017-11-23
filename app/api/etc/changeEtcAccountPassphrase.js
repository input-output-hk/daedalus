// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type ChangeEtcAccountPassphraseParams = {
  ca: string,
  walletId: string,
  oldPassphrase: string,
  newPassphrase: string,
};

export type ChangeEtcAccountPassphraseResponse = string;

export const changeEtcAccountPassphrase = (
  { ca, walletId, oldPassphrase, newPassphrase }: ChangeEtcAccountPassphraseParams
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
      oldPassphrase,
      newPassphrase
    ]
  })
);
