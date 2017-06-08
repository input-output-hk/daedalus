import BigNumber from 'bignumber.js';
import { RedeemAdaError } from '../errors';
import type {
  RedeemAdaRequest,
  RedeemPaperVendedAdaRequest,
} from '../index';
import CardanoClientApi from '../CardanoClientApi';

export default (api: CardanoClientApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = async (request: RedeemAdaRequest) => {
    const { walletId, redemptionCode } = request;
    const isValidAddress = await api.isValidAddress(walletId);
    const isValidRedemptionCode = await api.isValidRedemptionKey(redemptionCode);
    if (!isValidAddress || !isValidRedemptionCode) throw new RedeemAdaError();
    return { amount: new BigNumber(1000) };
  };

  api.redeemPaperVendedAda = async(request: RedeemPaperVendedAdaRequest) => {
    const { walletId, shieldedRedemptionKey, mnemonics } = request;
    const isValidKey = await api.isValidPaperVendRedemptionKey(shieldedRedemptionKey);
    const isValidAddress = await api.isValidAddress(walletId);
    const isValidMnemonic = await api.isValidRedemptionMnemonic(mnemonics);
    if (!isValidAddress || !isValidKey || !isValidMnemonic) {
      throw new RedeemAdaError();
    }
    return { amount: new BigNumber(1000) };
  };
};
