// @flow
import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

const { network } = global.environment;
const hostname = getNewsURL(network);
const path = '/newsfeed';
const filename = `newsfeed_${network}.json`;

export const getNews = (): Promise<string> =>
  externalRequest(
    {
      hostname,
      path: `${path}/${filename}`,
      method: 'GET',
      protocol: 'https',
    },
    true
  );
