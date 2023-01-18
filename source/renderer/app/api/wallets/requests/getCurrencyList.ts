import {
  genericCurrencyRequest,
  REQUESTS,
} from '../../../config/currencyConfig';

const requestName = REQUESTS.LIST;
export const getCurrencyList = genericCurrencyRequest(requestName);
