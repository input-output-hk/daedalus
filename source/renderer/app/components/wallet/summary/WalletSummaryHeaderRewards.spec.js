'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const WalletSummaryHeaderRewards_1 = require('./WalletSummaryHeaderRewards');
describe('getFormattedRewardAmount', () => {
  it('should render integer amounts without decimal places', () => {
    expect(
      (0, WalletSummaryHeaderRewards_1.getFormattedRewardAmount)(
        new bignumber_js_1.default(1)
      )
    ).toEqual('1 ADA');
  });
  it('should render fraction amounts with a single decimal place', () => {
    expect(
      (0, WalletSummaryHeaderRewards_1.getFormattedRewardAmount)(
        new bignumber_js_1.default(0.512)
      )
    ).toEqual('0.5 ADA');
  });
  it('should indicate values below 0.1', () => {
    expect(
      (0, WalletSummaryHeaderRewards_1.getFormattedRewardAmount)(
        new bignumber_js_1.default(0.05)
      )
    ).toEqual('< 0.1 ADA');
  });
});
describe('discreetRewardsAmount', () => {
  const symbol = '***';
  it('should replace the amount with a dash while restoring', () => {
    const replacer = (0, WalletSummaryHeaderRewards_1.discreetRewardsAmount)(
      true
    );
    expect(replacer(true, symbol, new bignumber_js_1.default(0))).toEqual('–');
  });
  it('should replace the amount with a discreet value', () => {
    const replacer = (0, WalletSummaryHeaderRewards_1.discreetRewardsAmount)();
    expect(replacer(true, symbol, new bignumber_js_1.default(0))).toEqual(
      '*** ADA'
    );
  });
  it('should return the formatted ADA value', () => {
    const replacer = (0, WalletSummaryHeaderRewards_1.discreetRewardsAmount)();
    expect(replacer(false, symbol, new bignumber_js_1.default(0.5))).toEqual(
      '0.5 ADA'
    );
  });
});
//# sourceMappingURL=WalletSummaryHeaderRewards.spec.js.map
