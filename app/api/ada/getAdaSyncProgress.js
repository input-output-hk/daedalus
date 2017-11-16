// @flow
import { request } from './lib/request';

export type GetAdaSyncProgressParams = {
  ca: string,
};

export type GetSyncProgressResponse = {
  _spLocalCD: {
    getChainDifficulty: {
      getBlockCount: number,
    }
  },
  _spNetworkCD: {
    getChainDifficulty: {
      getBlockCount: number,
    }
  },
  _spPeers: number,
};

export const getAdaSyncProgress = (
  { ca }: GetAdaSyncProgressParams
): Promise<GetSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/sync/progress',
    port: 8090,
    ca,
  })
);
