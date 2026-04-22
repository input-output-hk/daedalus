import { externalRequest } from '../../utils/externalRequest';
import {
  NEWS_HOSTNAME,
  NEWS_PROTOCOL,
  NEWS_PORT,
  NEWS_PATH_PREFIX,
} from '../../../config/urlsConfig';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const filename = isFlight
  ? 'newsfeed_mainnet_flight.json'
  : `newsfeed_${network}.json`;
const path = `${NEWS_PATH_PREFIX}/newsfeed/${filename}`;
export const newsUrl = `${NEWS_PROTOCOL}://${NEWS_HOSTNAME}${NEWS_PORT !== undefined ? `:${NEWS_PORT}` : ''}${path}`;
export const getNews = (): Promise<string> =>
  externalRequest(
    {
      hostname: NEWS_HOSTNAME,
      path,
      method: 'GET',
      protocol: NEWS_PROTOCOL,
      ...(NEWS_PORT !== undefined ? { port: NEWS_PORT } : {}),
    },
    true
  );
