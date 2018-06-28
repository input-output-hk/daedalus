// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type ApplyAdaUpdateParams = {
  ca: string,
};

export const applyAdaUpdate = (
  { ca }: ApplyAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
    port: environment.WALLET_PORT,
    ca,
  })
);
