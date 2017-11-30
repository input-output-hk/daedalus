import BigNumber from 'bignumber.js';
import { Logger } from '../../lib/logger';
import { RedeemAdaError } from '../errors';
import type {
  RedeemAdaRequest,
  RedeemPaperVendedAdaRequest,
} from '../index';
import CardanoClientApi from '../CardanoClientApi';

// ========== LOGGING =========

let LOCAL_TIME_DIFFERENCE = 0;

const stringifyData = (data) => JSON.stringify(data, null, 2);

export default (api: CardanoClientApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = async (request: RedeemAdaRequest) => {
    Logger.debug('CardanoClientApi::redeemAda (PATCHED) called: ' + stringifyData(request));
    const { redemptionCode } = request;
    const isValidRedemptionCode = await api.isValidRedemptionKey(redemptionCode);
    if (!isValidRedemptionCode) {
      Logger.debug('CardanoClientApi::redeemAda failed: not a valid redemption key!');
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };

  api.redeemPaperVendedAda = async(request: RedeemPaperVendedAdaRequest) => {
    Logger.debug('CardanoClientApi::redeemPaperVendedAda (PATCHED) called: ' + stringifyData(request));
    const { shieldedRedemptionKey, mnemonics } = request;
    const isValidKey = await api.isValidPaperVendRedemptionKey(shieldedRedemptionKey);
    const isValidMnemonic = await api.isValidRedemptionMnemonic(mnemonics);
    if (!isValidKey) Logger.debug('CardanoClientApi::redeemPaperVendedAda failed: not a valid redemption key!');
    if (!isValidMnemonic) Logger.debug('CardanoClientApi::redeemPaperVendedAda failed: not a valid mnemonic!');
    if (!isValidKey || !isValidMnemonic) {
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };

  api.getLocalTimeDifference = async () => {
    const response = Promise.resolve(LOCAL_TIME_DIFFERENCE);
    return response;
  };

  api.setLocalTimeDifference = async (timeDifference) => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
  };
};
