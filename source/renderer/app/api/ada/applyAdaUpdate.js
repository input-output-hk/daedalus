// @flow
import { request } from './lib/request';

export type ApplyAdaUpdateParams = {
};

export const applyAdaUpdate = (
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
  })
);
