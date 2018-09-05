// @flow
import type { AdaTransactionV1, RequestConfig } from './types';
import type { RedeemPaperVendedAdaRequest } from './';
import { redeemAda } from './redeemAda';

export const redeemAdaPaperVend = (
  config: RequestConfig,
  redemptionData: RedeemPaperVendedAdaRequest
): Promise<AdaTransactionV1> => redeemAda(config, redemptionData);
