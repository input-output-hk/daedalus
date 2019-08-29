// @flow
import { externalRequest } from '../../utils/externalRequest';
import { CHECK_INTERNET_CONNECTION_HOSTNAME } from '../../../config/urlsConfig';

const hostname = CHECK_INTERNET_CONNECTION_HOSTNAME;
const path = '/generate_204';

export const checkInternetConnection = (): Promise<string> =>
  externalRequest(
    {
      hostname,
      path,
      method: 'GET',
      protocol: 'https',
    },
    true
  );
