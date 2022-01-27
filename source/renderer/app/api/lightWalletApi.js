// @flow
import AdaApi from './api';
import { logger } from '../utils/logging';
import ApiError from '../domains/ApiError';


export default (api: AdaApi) => {
  api.tomoTest = async () => {
    return 'lightWalletAPI Test';
  };
};
