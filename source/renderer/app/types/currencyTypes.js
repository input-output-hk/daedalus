// @flow
import type { HttpOptions } from '../api/utils/externalRequest';

export type Currency = {
  symbol: string,
  name: string,
  decimalDigits?: number,
  id?: string,
};

export type Request = HttpOptions | Function;

export type RequestName = 'list' | 'rate';

export type CurrencyApiConfig = {
  id: string,
  name: string,
  hostname: string,
  website: string,
  requests: {
    list?: Request,
    rate: Request,
  },
  responses: {
    list: Function,
    rate: Function,
  },
};
