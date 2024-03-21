'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const discreetWalletAmount_1 = require('./discreetWalletAmount');
describe('discreetWalletAmount replacer', () => {
  it('should replace the given amount with the sensitive data symbol', () => {
    const amount = new bignumber_js_1.default(1);
    const discreetSymbol = '***';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      (0, discreetWalletAmount_1.discreetWalletAmount)({
        amount,
        withCurrency: false,
      })(true, discreetSymbol)
    ).toEqual(discreetSymbol);
  });
  it('should replace the amount and show the currency symbol', () => {
    const amount = new bignumber_js_1.default(1);
    const discreetSymbol = '***';
    const currency = 'TEST';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      (0, discreetWalletAmount_1.discreetWalletAmount)({
        amount,
        currency,
      })(true, discreetSymbol)
    ).toEqual(`${discreetSymbol} ${currency}`);
  });
  it('should show the amount with currency', () => {
    const discreetSymbol = '***';
    const amount = new bignumber_js_1.default(1);
    const currency = 'TEST';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      (0, discreetWalletAmount_1.discreetWalletAmount)({
        amount,
        currency,
      })(false, discreetSymbol)
    ).toEqual(`1.000000 ${currency}`);
  });
  it('should show the amount in short format', () => {
    const discreetSymbol = '***';
    const amount = new bignumber_js_1.default(1);
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      (0, discreetWalletAmount_1.discreetWalletAmount)({
        amount,
        withCurrency: false,
        long: false,
      })(false, discreetSymbol)
    ).toEqual('1');
  });
});
//# sourceMappingURL=discreetWalletAmount.spec.js.map
