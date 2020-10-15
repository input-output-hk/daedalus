// @flow
import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

const { isCatalyst, isFlight, environment } = global;
const { network } = environment;
const hostname = getNewsURL(network);
const path = '/newsfeed';

let filename = `newsfeed_${network}.json`;
if (isCatalyst) filename = 'newsfeed_catalyst.json';
if (isFlight) filename = 'newsfeed_mainnet_flight.json';

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
