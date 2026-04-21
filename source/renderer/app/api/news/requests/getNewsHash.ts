import { externalRequest } from '../../utils/externalRequest';
import {
  NEWS_HASH_HOSTNAME,
  NEWS_HASH_PROTOCOL,
  NEWS_HASH_PORT,
} from '../../../config/urlsConfig';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const path = isFlight
  ? '/newsfeed-verification/mainnet_flight'
  : `/newsfeed-verification/${network}`;
export const getNewsHash = (timestamp: number): Promise<string> =>
  externalRequest(
    {
      hostname: NEWS_HASH_HOSTNAME,
      path: `${path}/${timestamp}.txt`,
      method: 'GET',
      protocol: NEWS_HASH_PROTOCOL,
      ...(NEWS_HASH_PORT !== undefined ? { port: NEWS_HASH_PORT } : {}),
    },
    true
  );
