import BigNumber from 'bignumber.js';
import {
  discreetRewardsAmount,
  getFormattedRewardAmount,
} from './WalletSummaryHeaderRewards';

// @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'describe'. Do you need to instal... Remove this comment to see the full error message
describe('getFormattedRewardAmount', () => {
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should render integer amounts without decimal places', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(getFormattedRewardAmount(new BigNumber(1))).toEqual('1 ADA');
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should render fraction amounts with a single decimal place', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(getFormattedRewardAmount(new BigNumber(0.512))).toEqual('0.5 ADA');
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should indicate values below 0.1', () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(getFormattedRewardAmount(new BigNumber(0.05))).toEqual('< 0.1 ADA');
  });
});
// @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'describe'. Do you need to instal... Remove this comment to see the full error message
describe('discreetRewardsAmount', () => {
  const symbol = '***';
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should replace the amount with a dash while restoring', () => {
    const replacer = discreetRewardsAmount(true);
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(replacer(true, symbol, new BigNumber(0))).toEqual('-');
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should replace the amount with a discreet value', () => {
    const replacer = discreetRewardsAmount();
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(replacer(true, symbol, new BigNumber(0))).toEqual('*** ADA');
  });
  // @ts-ignore ts-migrate(2582) FIXME: Cannot find name 'it'. Do you need to install type... Remove this comment to see the full error message
  it('should return the formatted ADA value', () => {
    const replacer = discreetRewardsAmount();
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'expect'.
    expect(replacer(false, symbol, new BigNumber(0.5))).toEqual('0.5 ADA');
  });
});
