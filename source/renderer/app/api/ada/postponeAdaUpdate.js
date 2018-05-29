// @flow
import { request } from './lib/request';

export type PostponeAdaUpdateParams = {
};

export const postponeAdaUpdate = (
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/postpone',
  })
);
