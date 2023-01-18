import { externalRequest } from '../../utils/externalRequest';
import { getNewsHashURL } from '../../../utils/network';

// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const hostname = getNewsHashURL(network);
const path = isFlight
  ? '/newsfeed-verification/mainnet_flight'
  : `/newsfeed-verification/${network}`;
export const getNewsHash = (timestamp: number): Promise<string> =>
  externalRequest(
    {
      hostname,
      path: `${path}/${timestamp}.txt`,
      method: 'GET',
      protocol: 'https',
    },
    true
  );
