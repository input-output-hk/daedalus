// @flow
import type { DaedalusLatestVersionResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getLatestVersionInfoUrl } from '../../../utils/network';

const { isStaging, isTestnet, network } = global.environment;
const hostname = getLatestVersionInfoUrl(network);
const path = isStaging || isTestnet ? '' : '/update.cardano-mainnet.iohk.io';

export const getLatestAppVersion = (): Promise<DaedalusLatestVersionResponse> =>
  externalRequest({
    hostname,
    path: `${path}/daedalus-latest-version.json`,
    method: 'GET',
    protocol: 'https',
  });
