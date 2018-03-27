// @flow
import BigNumber from 'bignumber.js';

export type UnconfirmedAmount = {
  total: BigNumber,
  incoming: BigNumber,
  outgoing: BigNumber,
}
