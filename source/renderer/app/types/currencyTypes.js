// @flow
import type { HttpOptions } from '../api/utils/externalRequest';

export type Currency = {
  id: string,
  symbol: string,
  name: string,
  decimalDigits?: number,
};

export type Request = HttpOptions | Function;

export type RequestName = 'status' | 'list' | 'rate';

export type CurrencyApiConfig = {
  id: string,
  name: string,
  hostname: string,
  website: string,
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
