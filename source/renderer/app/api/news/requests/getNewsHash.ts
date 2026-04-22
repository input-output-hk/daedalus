import { externalRequest } from '../../utils/externalRequest';
import {
  NEWS_HASH_HOSTNAME,
  NEWS_HASH_PROTOCOL,
  NEWS_HASH_PORT,
  NEWS_HASH_PATH_PREFIX,
} from '../../../config/urlsConfig';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const pathPrefix = isFlight
  ? `${NEWS_HASH_PATH_PREFIX}/newsfeed-verification/mainnet_flight`
  : `${NEWS_HASH_PATH_PREFIX}/newsfeed-verification/${network}`;
export const getNewsHash = (timestamp: number): Promise<string> =>
  externalRequest(
    {
      hostname: NEWS_HASH_HOSTNAME,
      path: `${pathPrefix}/${timestamp}.txt`,
      method: 'GET',
      protocol: NEWS_HASH_PROTOCOL,
      ...(NEWS_HASH_PORT !== undefined ? { port: NEWS_HASH_PORT } : {}),
    },
    true
  );
