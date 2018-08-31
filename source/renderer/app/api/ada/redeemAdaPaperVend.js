// @flow
import type { AdaTransactionV1 } from './types';
import type { RedeemAdaParams } from './redeemAda';
import { redeemAda } from './redeemAda';

export type RedeemAdaPaperVendParams = {
  mnemonic: Array<string>,
  ...RedeemAdaParams
};

export const redeemAdaPaperVend = (
  params: RedeemAdaPaperVendParams
): Promise<AdaTransactionV1> => redeemAda(params);
