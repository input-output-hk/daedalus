// @flow
import { externalRequest } from '../../utils/externalRequest';
import { getNewsHashURL } from '../../../utils/network';

const { network } = global.environment;
const hostname = getNewsHashURL(network);
const path = `/newsfeed-verification/${network}`;

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
