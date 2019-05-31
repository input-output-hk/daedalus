// @flow
import type { DaedalusLatestVersionResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getLatesVersionInfoUrl } from '../../../utils/network';

const { NETWORK } = global.environment;
const url = getLatesVersionInfoUrl(NETWORK);

const urlChunks = url.split('://');
const protocol = urlChunks[0];
const location = urlChunks[1];
const hostname = location.split('/')[0];
const path = location.replace(hostname, '');

export const getLatestAppVersionInfo = (): Promise<DaedalusLatestVersionResponse> => (
  externalRequest({
    hostname,
    path: `${path}/daedalus-latest-version.json`,
    method: 'GET',
    protocol,
  })
);
