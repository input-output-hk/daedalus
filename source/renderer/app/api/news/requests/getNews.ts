import { externalRequest } from '../../utils/externalRequest';
import { getNewsURL } from '../../../utils/network';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const hostname = getNewsURL(network);
const path = '/newsfeed';
const filename = isFlight
  ? 'newsfeed_mainnet_flight.json'
  : `newsfeed_${network}.json`;
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
