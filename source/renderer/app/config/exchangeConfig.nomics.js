// @flow
import { AVAILABLE_APIS } from '../config/walletsConfig';

const { id, url: hostname } = AVAILABLE_APIS.NOMICS;

export default {
  id,
  status: {
    hostname,
    path: 'status',
  },
};
