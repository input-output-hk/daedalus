import BigNumber from 'bignumber.js';
import { Logger } from '../../../../common/logging';
import { RedeemAdaError } from '../errors';
import AdaApi from '../api';
import type {
  RedeemAdaParams,
  RedeemPaperVendedAdaParams
} from '../types';

// ========== LOGGING =========

let LOCAL_TIME_DIFFERENCE = 0;
let NEXT_ADA_UPDATE = null;

const stringifyData = (data) => JSON.stringify(data, null, 2);

export default (api: AdaApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = (request: RedeemAdaParams) => {
    Logger.debug('AdaApi::redeemAda (PATCHED) called: ' + stringifyData(request));
    const { redemptionCode } = request;
    const isValidRedemptionCode = api.isValidRedemptionKey(redemptionCode);
    if (!isValidRedemptionCode) {
      Logger.debug('AdaApi::redeemAda failed: not a valid redemption key!');
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };

  api.redeemPaperVendedAda = (request: RedeemPaperVendedAdaParams) => {
    Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) called: ' + stringifyData(request));
    const { shieldedRedemptionKey, mnemonics } = request;
    const isValidKey = api.isValidPaperVendRedemptionKey(shieldedRedemptionKey);
    const isValidMnemonic = api.isValidRedemptionMnemonic(mnemonics);
    if (!isValidKey) Logger.debug('AdaApi::redeemPaperVendedAda failed: not a valid redemption key!');
    if (!isValidMnemonic) Logger.debug('AdaApi::redeemPaperVendedAda failed: not a valid mnemonic!');
    if (!isValidKey || !isValidMnemonic) {
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };

  api.getLocalTimeDifference = async () => (
    Promise.resolve(LOCAL_TIME_DIFFERENCE)
  );

  api.setLocalTimeDifference = async (timeDifference) => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
  };

  api.nextUpdate = async () => (
    Promise.resolve(NEXT_ADA_UPDATE)
  );

  api.setNextUpdate = async (nextUpdate) => {
    NEXT_ADA_UPDATE = nextUpdate;
  };
};
