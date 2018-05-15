// @flow
import { request } from './lib/request';

export type ApplyAdaUpdateParams = {
  ca: string,
  port: number,
};

export const applyAdaUpdate = (
  { ca, port }: ApplyAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
    port,
    ca,
  })
);
