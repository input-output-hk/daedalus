// @flow
import { AVAILABLE_APIS } from '../config/walletsConfig';

const { id, url } = AVAILABLE_APIS.COINGECKO;
const version = 'v3';
const hostnameBase = `${url}/${version}`;

export default {
  id,
  status: {
    hostname: `${hostnameBase}/ping`,
  },
};
