// @flow
import { externalRequest } from '../../utils/externalRequest';
import exchangeConfig from '../../../config/exchangeConfig';

export const getCurrencyApiStatus = (): Promise<string> =>
  externalRequest(
    {
      method: 'GET',
      protocol: 'https',
      ...exchangeConfig.status,
    },
    true
  );
