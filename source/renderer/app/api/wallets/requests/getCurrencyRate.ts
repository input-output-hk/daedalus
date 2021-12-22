import {
  genericCurrencyRequest,
  REQUESTS,
} from '../../../config/currencyConfig';
import type { GetCurrencyRateRequest } from '../types';

const requestName = REQUESTS.RATE;
export const getCurrencyRate = (currency: GetCurrencyRateRequest) =>
  genericCurrencyRequest(requestName)(currency);
