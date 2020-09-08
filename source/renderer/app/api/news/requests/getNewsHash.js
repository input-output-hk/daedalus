// @flow
import { externalRequest } from '../../utils/externalRequest';
import { getNewsHashURL } from '../../../utils/network';

const { isFlight, isMainnetEAG, environment } = global;
const { network } = environment;
const hostname = getNewsHashURL(network);

let path = `/newsfeed-verification/${network}`;
if (isMainnetEAG) path = '/newsfeed-verification/mainnet_eag';
if (isFlight) path = '/newsfeed-verification/mainnet_flight';

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
