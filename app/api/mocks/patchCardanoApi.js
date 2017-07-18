import BigNumber from 'bignumber.js';
import Log from 'electron-log';
import { RedeemAdaError } from '../errors';
import type {
  RedeemAdaRequest,
  RedeemPaperVendedAdaRequest,
} from '../index';
import CardanoClientApi from '../CardanoClientApi';

// ========== LOGGING =========

const stringifyData = (data) => JSON.stringify(data, null, 2);

export default (api: CardanoClientApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = async (request: RedeemAdaRequest) => {
    Log.debug('CardanoClientApi::redeemAda (PATCHED) called', stringifyData(request));
    const { redemptionCode } = request;
    const isValidRedemptionCode = await api.isValidRedemptionKey(redemptionCode);
    if (!isValidRedemptionCode) {
      Log.debug('CardanoClientApi::redeemAda failed: not a valid redemption key!');
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };

  api.redeemPaperVendedAda = async(request: RedeemPaperVendedAdaRequest) => {
    Log.debug('CardanoClientApi::redeemPaperVendedAda (PATCHED) called', stringifyData(request));
    const { shieldedRedemptionKey, mnemonics } = request;
    const isValidKey = await api.isValidPaperVendRedemptionKey(shieldedRedemptionKey);
    const isValidMnemonic = await api.isValidRedemptionMnemonic(mnemonics);
    if (!isValidKey) Log.debug('CardanoClientApi::redeemPaperVendedAda failed: not a valid redemption key!');
    if (!isValidMnemonic) Log.debug('CardanoClientApi::redeemPaperVendedAda failed: not a valid mnemonic!');
    if (!isValidKey || !isValidMnemonic) {
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };
};
