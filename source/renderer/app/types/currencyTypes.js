// @flow
import type { HttpOptions } from '../api/utils/externalRequest';

export type Currency = {
  id: string,
  symbol: string,
  name: string,
};

export type Request = HttpOptions | Function;

export type RequestName = 'status' | 'list' | 'rate';

export type CurrencyApiConfig = {
  id: string,
  hostname: string,
  requests: {
    status: Request,
    list: Request,
    rate: Request,
  },
  responses: {
    status: Function,
    list: Function,
    rate: Function,
  },
};
