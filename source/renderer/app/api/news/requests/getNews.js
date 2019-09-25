// @flow
import type { GetNewsResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

const { network } = global.environment;
const hostname = getNewsURL(network);
const path = '/newsfeed';
const filename = `newsfeed_${network}.json`;

export const getNews = (): Promise<GetNewsResponse> =>
  externalRequest({
    hostname,
    path: `${path}/${filename}`,
    method: 'GET',
    protocol: 'https',
  });
