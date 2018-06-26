// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type NextAdaUpdateParams = {
  ca: string,
};

export const nextAdaUpdate = (
  { ca }: NextAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port: environment.WALLET_PORT,
    ca,
  })
);
