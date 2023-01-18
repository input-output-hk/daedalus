import type { HttpOptions } from '../api/utils/externalRequest';
import type { Locale } from '../../../common/types/locales.types';

export type Currency = {
  code: string;
  name: Record<Locale, string>;
  decimalDigits?: number;
  id?: string;
};
export type LocalizedCurrency = {
  code: string;
  name: string;
  decimalDigits?: number;
  id?: string;
};
export type DeprecatedCurrency = {
  symbol: string;
  name: string;
  decimalDigits?: number;
  id?: string;
};
export type Request = HttpOptions | ((...args: Array<any>) => any);
export type RequestName = 'list' | 'rate';
export type CurrencyApiConfig = {
  id: string;
  name: string;
  hostname: string;
  website: string;
  requests: {
    list?: Request;
    rate: Request;
  };
  responses: {
    list: (...args: Array<any>) => any;
    rate: (...args: Array<any>) => any;
  };
};
