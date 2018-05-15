// @flow
import { request } from './lib/request';

export type PostponeAdaUpdateParams = {
  ca: string,
  port: number,
};

export const postponeAdaUpdate = (
  { ca }: PostponeAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/postpone',
    port,
    ca,
  })
);
