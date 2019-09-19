// @flow
import type { GetNewsResponse } from '../types';
import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

const { isStaging, isTestnet, network } = global.environment;
const hostname = getNewsURL(network);
const path = isStaging || isTestnet ? '' : '/news-feed';

export const getNews = (): Promise<GetNewsResponse> =>
  externalRequest({
    hostname,
    path: `${path}/news.dummy.json`,
    method: 'GET',
    protocol: 'https',
  });
