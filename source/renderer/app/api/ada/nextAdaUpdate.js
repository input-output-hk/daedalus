// @flow
import { request } from './lib/request';

export type NextAdaUpdateParams = {
};

export const nextAdaUpdate = (
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
  })
);
