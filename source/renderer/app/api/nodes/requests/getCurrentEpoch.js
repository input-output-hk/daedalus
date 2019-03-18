// @flow
import type { CardanoExplorerResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getNetworkExplorerUri } from '../../../utils/network';

const { NETWORK, MAINNET } = global.environment;
const hostname = getNetworkExplorerUri(NETWORK);
const protocol = NETWORK === MAINNET ? 'https' : 'http';

export const getCurrentEpoch = (): Promise<CardanoExplorerResponse> =>
  externalRequest({
    hostname,
    path: '/api/blocks/pages',
    method: 'GET',
    protocol,
  });
