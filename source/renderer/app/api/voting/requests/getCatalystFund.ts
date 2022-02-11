import { externalRequest } from '../../utils/externalRequest';
import { MAINNET_SERVICING_STATION_URL } from '../../../config/urlsConfig';
import { GetCatalystFundResponse } from '../types';

export const getCatalystFund = (): Promise<GetCatalystFundResponse> =>
  externalRequest({
    hostname: MAINNET_SERVICING_STATION_URL,
    path: '/api/v0/fund',
    method: 'GET',
    protocol: 'https',
  });
