// @flow
import type { ApiWallets } from 'daedalus-client-api';
import { request } from './lib/request';

export const getAdaWallets = (ca: string): Promise<ApiWallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/wallets',
    port: 8090,
    ca,
  })
);
