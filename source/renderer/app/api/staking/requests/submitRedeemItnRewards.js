// @flow
// @REDEEM TODO: Remove when the API endpoint is implemented
/* eslint-disable */
import type { RequestConfig } from '../../common/types';
import type {
  RequestRedeemItnRewardsRequest,
  RequestRedeemItnRewardsResponse,
} from '../types';

const rewardsTotal = 1000;
const transactionFees = 1000;
const finalTotal = 1000;

export const requestRedeemItnRewards = ({
  walletId,
  recoveryPhrase,
}: RequestRedeemItnRewardsRequest): Promise<RequestRedeemItnRewardsResponse> =>
  Promise.resolve({
    rewardsTotal,
    transactionFees,
    finalTotal,
  });
