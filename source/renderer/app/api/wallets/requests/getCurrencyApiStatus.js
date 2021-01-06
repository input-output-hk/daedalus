// @flow
import {
  genericCurrencyRequest,
  REQUESTS,
} from '../../../config/currencyConfig';

const requestName = REQUESTS.STATUS;

export const getCurrencyApiStatus = genericCurrencyRequest(requestName);
