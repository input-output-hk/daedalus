// @flow
import type { HttpOptions } from '../api/utils/externalRequest';
import { Locale } from '../../../common/types/locales.types';

export type Currency = {
  code: string,
  name: string,
  decimalDigits?: number,
  id?: string,
};

export type RawCurrency = {
  code: {
    [key: Locale]: string,
  },
  name: {
    [key: Locale]: string,
  },
  decimalDigits?: number,
  id?: string,
};

export type LocalizedCurrency = {
  code: string,
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
