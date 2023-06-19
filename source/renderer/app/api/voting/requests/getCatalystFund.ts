import { externalRequest } from '../../utils/externalRequest';
import { CATALYST_API_URL } from '../../../config/urlsConfig';
import { GetCatalystFundResponse } from '../types';

export const getCatalystFund = (): Promise<GetCatalystFundResponse> => {
  const urlOverride: URL | undefined = new URL(
    environment.catalystApiUrlOverride
  );

  return externalRequest({
    hostname: urlOverride ? urlOverride.hostname : CATALYST_API_URL,
    path: '/api/v0/fund',
    method: 'GET',
    port: urlOverride ? Number(urlOverride.port) : undefined,
    protocol: urlOverride ? urlOverride.protocol.replace(':', '') : 'https',
  });
};
