// @flow
import type { ApiAccounts } from 'daedalus-client-api';
import { request } from './lib/request';

export const getAdaAccounts = (ca: string): Promise<ApiAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: 8090,
    ca,
  })
);
