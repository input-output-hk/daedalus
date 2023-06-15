import { externalRequest } from '../../utils/externalRequest';
import { CATALYST_API_URL } from '../../../config/urlsConfig';
import { GetCatalystFundResponse } from '../types';

export const getCatalystFund = (): Promise<GetCatalystFundResponse> =>
  externalRequest({
    hostname: CATALYST_API_URL,
    path: '/api/v0/fund',
    method: 'GET',
    protocol: 'https',
  });
