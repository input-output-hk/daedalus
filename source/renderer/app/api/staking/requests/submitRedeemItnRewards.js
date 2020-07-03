// @flow
// @REDEEM TODO:
/* eslint-disable */
import type { RequestConfig } from '../../common/types';
import type {
  SubmitRedeemItnRewardsRequest,
  SubmitRedeemItnRewardsResponse,
} from '../types';

const rewardsTotal = 1000;
const transactionFees = 1000;
const finalTotal = 1000;

export const submitRedeemItnRewards = ({
  walletId,
  recoveryPhrase,
}: SubmitRedeemItnRewardsRequest): Promise<SubmitRedeemItnRewardsResponse> =>
  Promise.resolve({
    rewardsTotal,
    transactionFees,
    finalTotal,
  });
