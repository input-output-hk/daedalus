import { externalRequest } from '../../utils/externalRequest';
import { NEWS_HOSTNAME, NEWS_PROTOCOL, NEWS_PORT } from '../../../config/urlsConfig';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const path = '/newsfeed';
const filename = isFlight
  ? 'newsfeed_mainnet_flight.json'
  : `newsfeed_${network}.json`;
export const getNews = (): Promise<string> =>
  externalRequest(
    {
      hostname: NEWS_HOSTNAME,
      path: `${path}/${filename}`,
      method: 'GET',
      protocol: NEWS_PROTOCOL,
      ...(NEWS_PORT !== undefined ? { port: NEWS_PORT } : {}),
    },
    true
  );
