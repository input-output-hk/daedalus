import BigNumber from 'bignumber.js';
import {
  discreetRewardsAmount,
  getFormattedRewardAmount,
} from './WalletSummaryHeaderRewards';

describe('getFormattedRewardAmount', () => {
  it('should render integer amounts without decimal places', () => {
    expect(getFormattedRewardAmount(new BigNumber(1))).toEqual('1 ADA');
  });
  it('should render fraction amounts with a single decimal place', () => {
    expect(getFormattedRewardAmount(new BigNumber(0.512))).toEqual('0.5 ADA');
  });
  it('should indicate values below 0.1', () => {
    expect(getFormattedRewardAmount(new BigNumber(0.05))).toEqual('< 0.1 ADA');
  });
});
describe('discreetRewardsAmount', () => {
  const symbol = '***';
  it('should replace the amount with a dash while restoring', () => {
    const replacer = discreetRewardsAmount(true);
    expect(replacer(true, symbol, new BigNumber(0))).toEqual('–');
  });
  it('should replace the amount with a discreet value', () => {
    const replacer = discreetRewardsAmount();
    expect(replacer(true, symbol, new BigNumber(0))).toEqual('*** ADA');
  });
  it('should return the formatted ADA value', () => {
    const replacer = discreetRewardsAmount();
    expect(replacer(false, symbol, new BigNumber(0.5))).toEqual('0.5 ADA');
  });
});
