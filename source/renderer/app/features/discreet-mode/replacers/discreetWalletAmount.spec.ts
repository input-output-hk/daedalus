import BigNumber from 'bignumber.js';
import { discreetWalletAmount } from './discreetWalletAmount';

describe('discreetWalletAmount replacer', () => {
  it('should replace the given amount with the sensitive data symbol', () => {
    const amount = new BigNumber(1);
    const discreetSymbol = '***';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      discreetWalletAmount({
        amount,
        withCurrency: false,
      })(true, discreetSymbol)
    ).toEqual(discreetSymbol);
  });
  it('should replace the amount and show the currency symbol', () => {
    const amount = new BigNumber(1);
    const discreetSymbol = '***';
    const currency = 'TEST';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      discreetWalletAmount({
        amount,
        currency,
      })(true, discreetSymbol)
    ).toEqual(`${discreetSymbol} ${currency}`);
  });
  it('should show the amount with currency', () => {
    const discreetSymbol = '***';
    const amount = new BigNumber(1);
    const currency = 'TEST';
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      discreetWalletAmount({
        amount,
        currency,
      })(false, discreetSymbol)
    ).toEqual(`1.000000 ${currency}`);
  });
  it('should show the amount in short format', () => {
    const discreetSymbol = '***';
    const amount = new BigNumber(1);
    expect(
      // @ts-ignore ts-migrate(2554) FIXME: Expected 3 arguments, but got 2.
      discreetWalletAmount({
        amount,
        withCurrency: false,
        long: false,
      })(false, discreetSymbol)
    ).toEqual('1');
  });
});
