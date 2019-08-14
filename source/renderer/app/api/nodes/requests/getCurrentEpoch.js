// @flow
import type { CardanoExplorerResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getNetworkExplorerUri } from '../../../utils/network';

const { isStaging, isTestnet, network } = global.environment;
const hostname = getNetworkExplorerUri(network);
const protocol = isStaging || isTestnet ? 'http' : 'https';

export const getCurrentEpoch = (): Promise<CardanoExplorerResponse> =>
  externalRequest({
    hostname,
    path: '/api/blocks/pages',
    method: 'GET',
    protocol,
  });
