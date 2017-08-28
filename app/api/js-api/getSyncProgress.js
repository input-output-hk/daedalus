// @flow
import { Request } from './lib/Request';

export type GetSyncProgressParams = void;

export type GetSyncProgressResponse = {
  Right: {
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
  }
};

export type GetSyncProgressRequest = Request<GetSyncProgressParams, GetSyncProgressResponse>;
