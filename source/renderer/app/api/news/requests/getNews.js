// @flow
import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

const { isFlight, isMainnetEAG, environment } = global;
const { network } = environment;
const hostname = getNewsURL(network);
const path = '/newsfeed';

let filename = `newsfeed_${network}.json`;
if (isFlight) filename = 'newsfeed_mainnet_flight.json';
if (isMainnetEAG) filename = 'newsfeed_mainnet_eag.json';

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
