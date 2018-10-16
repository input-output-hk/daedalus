// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type PostponeAdaUpdateParams = {
  ca: string,
};

export const postponeAdaUpdate = (
  { ca }: PostponeAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/postpone',
    port: environment.WALLET_PORT,
    ca,
  })
);
